{application, ifastcgi, 
      [
      {description, "A FastCGI Server"},
      {mod, {ifastcgi_app, []}},
      {env, [{config_file, "etc/ifastcgi.conf"}]},
      {modules, [kernel, stdlib, sasl]}
      ]}.
