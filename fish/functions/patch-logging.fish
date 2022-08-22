function patch-logging
    for mod in $argv[1..-1]
        if test "core-api" = $mod
            sudo cp /home/antoine/m3-logging-api_2.13-FORK.STABLE.jar /usr/lib/macq/m3/core-api/lib/eu.macq.m3-logging-api_2.13-2.0.0.jar
            sudo cp /home/antoine/m3-logging-encoder_2.13-FORK.STABLE.jar /usr/lib/macq/m3/core-api/lib/eu.macq.m3-logging-encoder_2.13-2.0.0.jar
        else
            sudo cp /home/antoine/m3-logging-api_2.13-FORK.STABLE.jar /usr/lib/macq/m3/$mod/main/lib/eu.macq.m3-logging-api_2.13-2.0.0.jar
            sudo cp /home/antoine/m3-logging-encoder_2.13-FORK.STABLE.jar /usr/lib/macq/m3/$mod/main/lib/eu.macq.m3-logging-encoder_2.13-2.0.0.jar
        end
    end
end
