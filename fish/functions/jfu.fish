function jfu
    echo 'journalctl -f' '-u macq-m3-'$argv '-n 120'
    journalctl -fu "macq-m3-"$argv -n 120
end
