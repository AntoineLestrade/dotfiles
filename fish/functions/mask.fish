function mask
    systemctl daemon-reload
    systemctl stop "macq-m3-"$argv[1..-1]
    systemctl mask "macq-m3-"$argv[1..-1]
end
