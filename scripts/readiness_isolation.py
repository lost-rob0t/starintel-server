#!/usr/bin/env python3
"""Exercise a disposable, private local server's tenant boundary over HTTP."""
import argparse
import json
import time
from pathlib import Path
from urllib.error import HTTPError
from urllib.parse import quote, urlencode, urlsplit
from urllib.request import HTTPRedirectHandler, Request, build_opener


class NoRedirect(HTTPRedirectHandler):
    def redirect_request(self, *args, **kwargs):
        return None


class Client:
    def __init__(self, base, key):
        url = urlsplit(base)
        if (url.scheme != "http" or url.hostname not in ("localhost", "127.0.0.1", "::1")
                or url.username or url.password or url.query or url.fragment):
            raise ValueError("use a disposable loopback HTTP server")
        self.base, self.key = base.rstrip("/"), key
        self.opener = build_opener(NoRedirect())

    def request(self, method, path, body=None):
        data = json.dumps(body).encode() if body is not None else None
        request = Request(self.base + path, data=data, method=method,
                          headers={"Authorization": "Bearer " + self.key,
                                   "Content-Type": "application/json"})
        try:
            response = self.opener.open(request, timeout=15)
        except HTTPError as error:
            response = error
        with response:
            raw = response.read(4 * 1024 * 1024 + 1)
            if len(raw) > 4 * 1024 * 1024:
                raise AssertionError("response exceeds probe byte budget")
            try:
                value = json.loads(raw)
            except (ValueError, UnicodeError):
                value = None
            return response.status, value


def expect(client, method, path, statuses, body=None):
    status, result = client.request(method, path, body)
    if status not in statuses:
        # Never include response bodies, headers or credential material.
        raise AssertionError(f"{method} {path.split('?')[0]}: expected {statuses}, got {status}")
    return result


def identifiers(value):
    if isinstance(value, dict):
        found = {value[key] for key in ("id", "_id") if isinstance(value.get(key), str)}
        return found.union(*(identifiers(item) for item in value.values()))
    if isinstance(value, list):
        return set().union(*(identifiers(item) for item in value))
    return set()


def validate_inputs(documents, tenants):
    if len(set(tenants)) != 2 or not all(tenants):
        raise ValueError("two distinct nonempty tenants required")
    ids = [doc.get("id", doc.get("_id", "")) for doc in documents]
    if len(set(ids)) != 2 or not all(i.startswith(("readiness-", "readiness:")) for i in ids):
        raise ValueError("two unique readiness fixture document IDs required")
    if documents[0]["dataset"] == documents[1]["dataset"]:
        raise ValueError("use distinct datasets mapped to the two tenants")
    if any("tenant_id" in doc for doc in documents):
        raise ValueError("tenant assignment must use server dataset mapping")
    return ids


def run(clients, documents, tenants, wait_seconds=60):
    ids = validate_inputs(documents, tenants)
    paths = ["/document/" + quote(i, safe="") for i in ids]
    # Both credentials must have dataset:* so these are tenant-only denials.
    for index, client in enumerate(clients):
        context = expect(client, "GET", "/auth/context", {200})
        scopes = set(context.get("scopes", []))
        required = {"identity:read", "documents:read", "documents:write",
                    "documents:delete", "search:read", "dataset:*", "tenant:" + tenants[index]}
        if scopes != required:
            raise AssertionError("credential scopes must exactly match the tenant probe contract")
        expect(client, "GET", paths[index], {404})
        expect(client, "POST", "/new/document/" + quote(documents[index]["dtype"], safe=""),
               {200, 201, 202}, documents[index])
    deadline = time.monotonic() + wait_seconds
    for index, client in enumerate(clients):
        while True:
            status, value = client.request("GET", paths[index])
            if status == 200 and ids[index] in identifiers(value):
                break
            if status not in (200, 404) or time.monotonic() >= deadline:
                raise AssertionError("own-tenant ingestion did not become readable")
            time.sleep(0.25)
    for index, client in enumerate(clients):
        other = 1 - index
        expect(client, "GET", paths[other], {403})
        expect(client, "PUT", paths[other], {403}, documents[other])
        expect(client, "DELETE", paths[other], {403})
        expect(client, "POST", "/new/document/" + quote(documents[other]["dtype"], safe=""),
               {403}, documents[other])
        # Reject leaked CouchDB surfaces; do not require new application APIs.
        for path in ("/_changes", "/documents/_changes", paths[other] + "/fixture.txt",
                     paths[other] + "/_attachments/fixture.txt"):
            expect(client, "GET", path, {403, 404, 405})
        query = {"q": "*:*", "tenant": tenants[index], "limit": 100}
        while True:
            result = expect(client, "GET", "/search?" + urlencode(query), {200})
            found = identifiers(result)
            if ids[other] in found:
                raise AssertionError("cross-tenant search disclosure")
            if ids[index] in found:
                break
            if time.monotonic() >= deadline:
                raise AssertionError("own-tenant search fixture absent")
            time.sleep(0.25)
        query["tenant"] = tenants[other]
        expect(client, "GET", "/search?" + urlencode(query), {403})
        query.update(tenant=tenants[index], limit=101)
        expect(client, "GET", "/search?" + urlencode(query), {400})
        expect(client, "POST", "/new/document/" + quote(documents[index]["dtype"], safe=""),
               {413}, {"padding": "x" * (1024 * 1024 + 1)})
    # Verify rejected mutations did not damage either acknowledged record.
    for index, client in enumerate(clients):
        value = expect(client, "GET", paths[index], {200})
        if ids[index] not in identifiers(value):
            raise AssertionError("acknowledged document disappeared")
    return {"status": "passed", "tenants": 2, "live_http": True,
            "limits_proven": ["result limit 101 rejected", "JSON body over 1 MiB rejected"],
            "limits_unproven": ["transport body buffering", "backend query work", "query deadlines"]}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--url", required=True)
    parser.add_argument("--key-file", action="append", required=True)
    parser.add_argument("--fixture", action="append", required=True)
    parser.add_argument("--tenant", action="append", required=True)
    args = parser.parse_args()
    if any(len(values) != 2 for values in (args.key_file, args.fixture, args.tenant)):
        parser.error("provide exactly two keys, fixtures and tenants, in matching order")
    clients = [Client(args.url, Path(path).read_text().strip()) for path in args.key_file]
    documents = [json.loads(Path(path).read_text()) for path in args.fixture]
    try:
        print(json.dumps(run(clients, documents, args.tenant), sort_keys=True))
    except Exception as error:
        # Untrusted HTTP exceptions may contain server-controlled content.
        print(json.dumps({"status": "failed", "error_type": type(error).__name__}))
        raise SystemExit(1)


if __name__ == "__main__":
    main()
