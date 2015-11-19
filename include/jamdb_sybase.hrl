-record(format, {
    column_name = <<>>,
    status,
    usertype,
    datatype,
    datatype_group,
    datatype_max_len,
    datatype_precision,
    datatype_scale,
    datatype_locale,
    datatype_class_id,
    datatype_name,
    db_name,
    owner_name,
    table_name,
    label_name  = <<>>
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
