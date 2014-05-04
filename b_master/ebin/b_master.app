{application, b_master,
    [{vsn, "1.0.0"},
    {description, "Handle requests, create workers and cache results"},
    {modules, [b_master, b_master_sup, b_service_broker, b_worker, b_cache_sup, b_cache, b_cache_cleaner]},
    {registered, [b_master_sup, b_service_broker, b_cache_sup, b_cache, b_cache_cleaner]},
    {mod, {b_master, []}},
    {env, [
        {cache_size, 200},
        {dead_worker_timeout, 10000000},
        {worker_timeout, 4400000},
        {sup_maxtime, 1},
        {sup_maxrestart, 1000},
        {sup_shutdown, 5000},
        {subsup_shutdown, 4500},
        {cache_clean_timeout, 5000}
    ]},
    {applications, [kernel, stdlib]}
]}.