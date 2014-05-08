{application, b_balancer,
    [{vsn, "1.0.0"},
    {description, "Balance load between active service servers"},
    {modules, [b_balancer, b_balancer_sup, b_load_balancer]},
    {registered, [b_balancer_sup]},
    {mod, {b_balancer, []}},
    {env, [
        {sup_shutdown, 5000},
        {sup_maxtime, 1},
        {sup_maxrestart, 1000},
        {basic_server_capacity, 1000},
        {srv_requests_per_interval, 2},
        {replication_interval, 500},
        {replication_top, 0.9}
    ]},
    {applications, [kernel, stdlib]}
]}.