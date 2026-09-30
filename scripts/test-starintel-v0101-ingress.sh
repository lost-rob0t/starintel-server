#!/usr/bin/env bash
set -euo pipefail

cache_dir=/tmp/starintel-server-asdf-cache
mkdir -p "$cache_dir"

env -u 'BASH_FUNC_git-sync%%' \
  nix develop --override-input star-cl path:../star-cl --command \
  sbcl --non-interactive --no-userinit --no-sysinit \
  --eval '(require :asdf)' \
  --eval "(asdf:initialize-output-translations '(:output-translations (t \"$cache_dir/\") :ignore-inherited-configuration))" \
  --eval '(asdf:load-system :starintel-gserver)' \
  --eval '(let* ((collision (jsown:parse "{\"_id\":\"old\",\"id\":\"new\",\"dataset\":\"fixture\",\"dtype\":\"person\",\"schema_version\":\"0.9.0\"}")) (valid (jsown:parse "{\"_id\":\"person-valid\",\"dataset\":\"fixture\",\"dtype\":\"person\",\"schema_version\":\"0.9.0\",\"data\":{\"fname\":\"Ada\",\"external_ids\":{\"passport\":\"A123\"}}}"))) (multiple-value-bind (documents quarantine) (star.frontends.http-api:canonical-document-batch (list collision valid)) (assert (= 2 (length documents))) (assert (= 1 (length quarantine))) (assert (string= "ambiguousFieldCollision" (jsown:val (aref quarantine 0) "reasonCode"))) (assert (string= "person-valid" (jsown:val (first documents) "id"))) (assert (string= "0.10.1" (jsown:val (first documents) "schemaVersion"))) (assert (not (jsown:keyp (first documents) "_id"))) (assert (not (jsown:keyp (first documents) "schema_version"))) (assert (string= "person-identifier" (jsown:val (second documents) "dtype"))) (let* ((wire (star.documents:document-json (first documents))) (parsed (jsown:parse wire)) (stored (star.rabbit:decode-rabbit-document (cons wire 1)))) (assert (not (jsown:keyp parsed "_id"))) (assert (string= "person-valid" (jsown:val stored "_id")))) (format t "StarIntel 0.10.1 server ingress smoke passed~%")))' \
  --eval '(uiop:quit 0)'
