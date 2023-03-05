function refresh-conf
    ansible-playbook -i /usr/lib/macq/m3/deployer/hosts-m3antoine /usr/lib/macq/m3/deployer/roles/install-upgrade.yml --tags configuration
end
