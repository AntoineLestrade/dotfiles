function rld --wraps='sudo systemctl daemon-reload; sudo systemctl reload nginx' --description 'alias rld=sudo systemctl daemon-reload; sudo systemctl reload nginx'
  sudo systemctl daemon-reload; sudo systemctl reload nginx ;
end
