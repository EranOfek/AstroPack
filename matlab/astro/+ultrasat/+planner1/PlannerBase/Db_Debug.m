% Postgres test
% Chen Tishler, 04/08/2024

d = Db();
query = 'SELECT pk, alert_id, alert_superevent_id FROM incoming_alerts.alerts LIMIT 5';
query = 'SELECT pk, alert_id, alert_superevent_id, alert_rcv_time FROM incoming_alerts.alerts LIMIT 5';
query = 'SELECT * FROM incoming_alerts.alerts LIMIT 5';
query = 'SELECT alert_rcv_time FROM incoming_alerts.alerts LIMIT 5';
data = d.select(query);
disp(data);
