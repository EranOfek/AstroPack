
function Result = unitTestServerSide()
	% Simple test (without header) - write current time string to shared memory

	io.msgLog(LogLevel.Test, 'cpp side (server)')
	sm = SharedMemory()
	sm.open(FILENAME)
	while true
		msg = datetime.datetime.now().strftime('%d/%m/%y %H:%M:%S.%f')  # [:-3]
		print(msg)
		buf = bytes(msg, 'ascii')
		sm.write(0, buf)
		time.sleep(0.1)
	end
end

