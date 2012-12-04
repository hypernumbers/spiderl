{application,spiderl,
             [{description,["a simple erlang spider"]},
              {vsn,"aaaaa"},
              {registered,[]},
              {applications,[kernel,stdlib]},
              {mod,{spiderl_app,[]}},
              {env,[]},
              {modules,[
                        start,
                        spiderl_app,
                        spiderl_sup,
                        spiderl_srv,
                        parse_csv,
                        parse_html,
                        hslists
                       ]
              }]}.
