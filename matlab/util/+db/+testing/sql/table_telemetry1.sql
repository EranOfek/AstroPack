
CREATE TABLE public.telemetry1 (
pk1 BIGINT GENERATED ALWAYS AS IDENTITY,

rcv_time DOUBLE PRECISION DEFAULT 0,
param VARCHAR,
idx INTEGER,
f_time DOUBLE PRECISION DEFAULT 0,
f_tick BIGINT,
f_value DOUBLE PRECISION DEFAULT 0,
s_value VARCHAR

);

CREATE INDEX telemetry1_rcv_time ON public.telemetry1
  USING btree (rcv_time);
    
CREATE INDEX telemetry1_f_time ON public.telemetry1
  USING btree (f_time);
  
CREATE INDEX telemetry1_param ON public.telemetry1
  USING btree (param);  
  
CREATE INDEX telemetry1_f_value ON public.telemetry1
  USING btree (f_value);

CREATE INDEX telemetry1_s_value ON public.telemetry1
  USING btree (s_value);

