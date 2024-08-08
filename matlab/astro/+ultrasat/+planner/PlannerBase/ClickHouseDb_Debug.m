% ClickHouse test
% Chen Tishler, 04/08/2024

d = ClickHouseDb();
query = 'SELECT src_img_pk, src_idx, src_time FROM sources_test.sources1hp LIMIT 10';
query = 'SELECT src_time FROM sources_test.sources1hp LIMIT 10 OFFSET 100000';
query = 'SELECT * FROM sources_test.sources1hp LIMIT 1 OFFSET 100000';
data = d.select(query);
disp(data);
