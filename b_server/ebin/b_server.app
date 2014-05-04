{application, b_server,
    [{vsn, "1.0.0"},
    {description, "Compute services results"},
    {modules, [b_service_server, b_server, b_server_sup, service_caching, service_content,
                service_header, service_infected, service_timeouted]},
    {registered, [b_server_sup]},
    {mod, {b_server, []}},
    {env, [
        {sup_shutdown, 5000},
        {sup_maxtime, 1},
        {sup_maxrestart, 1000}
    ]},
    {applications, [kernel, stdlib]}
]}.