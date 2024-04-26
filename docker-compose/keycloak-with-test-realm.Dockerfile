FROM keycloak/keycloak:24.0.3
COPY ./realm.json /opt/keycloak/data/import/realm.json

COPY ./keycloak-entrypoint.sh /entrypoint.sh
USER root
RUN chmod +x /entrypoint.sh
USER 1000

ENTRYPOINT ["/bin/sh", "-c"]
CMD ["/entrypoint.sh"]