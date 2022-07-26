function rst
    sudo systemctl daemon-reload
    sudo systemctl unmask "macq-m3-"$argv[1..-1]
    sudo systemctl restart "macq-m3-"$argv[1..-1]
end
