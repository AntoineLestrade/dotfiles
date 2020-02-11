function tbx
    set cmd $argv[1]
    switch $cmd
        case start
            toolbox module start "macq-m3-"$argv[2..-1]
        case restart
            toolbox module restart "macq-m3-"$argv[2..-1]
        case stop
            toolbox module stop "macq-m3-"$argv[2..-1]
        case mongo
            toolbox mongo create
        case kafka
            toolbox kafka create-missing
        case keycloak
            toolbox keycloak initialize
        case '*'
            echo 'ERROR: Unknown command '$cmd
    end
end
