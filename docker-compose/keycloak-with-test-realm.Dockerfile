FROM keycloak/keycloak:22.0.5
COPY ./realm.json /opt/keycloak/data/import/realm.json

CMD ["start-dev --import-realm --features=\"admin-fine-grained-authz\""]