{application, b_proxy,
    [{vsn, "1.0.0"},
    {modules, [b_proxy]},
    {registered, [b_proxy]},
    {env, [
        {yapp_appmods,[{"b_thesis",b_proxy}]},
        {request_timeout, 4500}
    ]},
    {applications, [kernel, stdlib]}
]}.