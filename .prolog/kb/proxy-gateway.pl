% Issue 17: core proxy actor authority and Evomi provider limits.
% Verified by proxy-gateway-tests; provider behavior from Evomi docs
% retrieved 2026-09-16 (see doc/proxy-gateway.org).
actor_boundary(proxy_gateway, sento, internal_request_reply).
actor_boundary(evomi_proxy_provider, sento, provider_http_effect).
provider_mode(evomi, rotating, no_session_suffix).
provider_mode(evomi, sticky, best_effort_session).
provider_limit(evomi, sticky_lifetime_minutes, 1, 1440).
provider_unsupported(evomi, vpn_configuration).
secret_boundary(evomi_api_key, provider_header_only).
secret_boundary(proxy_credentials, actor_reply_only).
