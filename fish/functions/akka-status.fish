function akka-status
    set -l port 25610
    command curl -s http://m3antoine:$port/cluster/members | jq .
end
