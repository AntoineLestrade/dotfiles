function refresh-conf
    ansible-playbook -i /usr/lib/macq/m3/bulma/hosts-m3antoine /usr/lib/macq/m3/bulma/roles/install-upgrade.yml --tags m3structure
end
