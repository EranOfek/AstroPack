function Result = unitTestClientSide()
	% Simple test - read string from shared memory
	log('python side (client)')
	sm = SharedMemory()
	sm.open(FILENAME)
	while true
		buf = sm.read(0, 100)
		if len(buf) > 0
			s = buf.decode('ascii')
			s = s.replace('\x00', '')
			if len(s) > 0
				log('rcv: ' + s)
			end
		end
		time.sleep(0.01)
	end
end

