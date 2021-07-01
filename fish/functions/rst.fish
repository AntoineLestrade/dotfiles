function rst
    systemctl daemon-reload
    systemctl unmask "macq-m3-"$argv[1..-1]
    systemctl restart "macq-m3-"$argv[1..-1]
end
