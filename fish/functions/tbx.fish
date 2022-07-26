function tbx
    set cmd $argv[1]
    switch $cmd
        case start
            sudo m3-toolbox module start "macq-m3-"$argv[2..-1]
        case restart
            sudo m3-toolbox module restart "macq-m3-"$argv[2..-1]
        case stop
            sudo m3-toolbox module stop "macq-m3-"$argv[2..-1]
        case mongo
            sudo m3-toolbox mongo create
            mongo -u antoine -p antoine --eval 'db.getSiblingDB("m3config").schedulerConfigurations.update({id: "iCar_recovery"}, { $set: { enabled: false } })'
        case kafka
            sudo m3-toolbox kafka create
        case keycloak
            sudo m3-toolbox keycloak clean --adm-user macq --adm-password itsobvious
            sudo m3-toolbox keycloak initialize --adm-user macq --adm-password itsobvious
        case setup
            sudo m3-toolbox mongo create
            sudo m3-toolbox kafka create
            sudo m3-toolbox keycloak clean --adm-user macq --adm-password itsobvious
            sudo m3-toolbox keycloak initialize --adm-user macq --adm-password itsobvious
        case '*'
            echo 'ERROR: Unknown command '$cmd
    end
end
