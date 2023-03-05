function mask
    sudo systemctl daemon-reload
    sudo systemctl stop "macq-m3-"$argv[1..-1]
    sudo systemctl mask "macq-m3-"$argv[1..-1]
end
