function macq-status
    systemctl list-units | grep --color=never -e macq-m3 -e keycloak -e nginx -e zookeeper -e kafka -e mongo | ack --color-match=GREEN --passthru running
end
