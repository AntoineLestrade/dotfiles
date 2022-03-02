function tbx
    set cmd $argv[1]
    switch $cmd
        case start
            m3-toolbox module start "macq-m3-"$argv[2..-1]
        case restart
            m3-toolbox module restart "macq-m3-"$argv[2..-1]
        case stop
            m3-toolbox module stop "macq-m3-"$argv[2..-1]
        case mongo
            m3-toolbox mongo create
            mongo -u antoine -p antoine --eval 'db.getSiblingDB("m3config").schedulerConfigurations.update({id: "iCar_recovery"}, { $set: { enabled: false } })'
        case kafka
            m3-toolbox kafka create
        case keycloak
            m3-toolbox keycloak clean --adm-user macq --adm-password itsobvious
            m3-toolbox keycloak initialize --adm-user macq --adm-password itsobvious
        case setup
            m3-toolbox mongo create
            m3-toolbox kafka create
            m3-toolbox keycloak clean --adm-user macq --adm-password itsobvious
            m3-toolbox keycloak initialize --adm-user macq --adm-password itsobvious
        case '*'
            echo 'ERROR: Unknown command '$cmd
    end
end
