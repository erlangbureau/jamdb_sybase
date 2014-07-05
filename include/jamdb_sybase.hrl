-record(format, {
    format :: fixed | variable | long | text | decimal,
    usertype,
    tdstype,
    status,
    db_name,
    owner_name,
    table_name,
    label_name,
    column_name,
    obj_name,
    class_id,
    scale,
    locale
}).

-record(message, {
        msg_number, 
        msg_state, 
        class, 
        sql_state,
        status, 
        transaction_state, 
        msg_body, 
        server_name, 
        procedure_name, 
        line_number
}).
