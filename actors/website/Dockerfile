FROM python:3.12-bookworm

ARG PLAYWRIGHT_VERSION=1.62.0
ENV PLAYWRIGHT_BROWSERS_PATH=/ms-playwright \
    PYTHONUNBUFFERED=1 \
    PYTHONDONTWRITEBYTECODE=1

LABEL org.opencontainers.image.title="StarIntel Website Actor" \
      org.opencontainers.image.source="https://github.com/lost-rob0t/starintel-server" \
      org.opencontainers.image.description="Extensible StarIntel website actor with Playwright Chromium"

RUN python -m pip install --no-cache-dir "playwright==${PLAYWRIGHT_VERSION}" \
    && python -m playwright install --with-deps chromium \
    && useradd --create-home --uid 10001 --shell /usr/sbin/nologin staractor \
    && chmod -R a+rX /ms-playwright

WORKDIR /app
COPY pyproject.toml /app/pyproject.toml
COPY src /app/src
RUN python -m pip install --no-cache-dir . \
    && chown -R staractor:staractor /app

USER staractor
ENTRYPOINT ["starintel-web-actor"]
CMD ["worker", "wef-ygl"]
