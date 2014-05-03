{application, b_server,
    [{vsn, "1.0.0"},
    {modules, [b_service_server, b_server, b_server_sup, service_caching, service_content,
                service_header, service_infected, service_timeouted]},
    {registered, [b_server_sup]},
    {mod, {b_server, []}},
    {env, [
        {sup_shutdown, 5000},
        {sup_maxtime, 1000},
        {sup_maxrestart, 100000}
    ]},
    {applications, [kernel, stdlib]}
]}.