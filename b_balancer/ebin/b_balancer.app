{application, b_balancer,
    [{vsn, "1.0.0"},
    {modules, [b_balancer, b_balancer_sup, b_load_balancer]},
    {registered, [b_balancer_sup]},
    {mod, {b_balancer, []}},
    {env, [
        {sup_shutdown, 5000},
        {sup_maxtime, 1000},
        {sup_maxrestart, 100000},
        {basic_server_capacity, 1000},
        {srv_requests_per_interval, 100000},
        {replication_interval, 1000000},
        {replication_top, 0.9},
        {replication_bottom, 0.5}
    ]},
    {applications, [kernel, stdlib]}
]}.