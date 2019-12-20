function macq-status
    systemctl list-unit-files | grep --color=never -e macq-m3 -e keycloak -e nginx -e zookeeper -e kafka | ack --color-match=GREEN --passthru enabled
end
