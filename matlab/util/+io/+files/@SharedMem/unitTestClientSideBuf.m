function Result = unitTestClientSideBuf()
	% Read next buffer from shared-memory queue
	log('python side (client)')
	sm = SharedMemory()
	sm.open(FILENAME)
	last_put_counter = 0
	while true
		buf, put_counter, w, h = sm.get_buf()
		if buf
			print('get_buf: put_counter: {}, len: {}, w: {}, h: {}, buf[0]: {}'.format(put_counter, len(buf), w, h, buf[0]))
			if put_counter != last_put_counter+1:
				print('GAP IN put_counter: {}, last: {}'.format(put_counter, last_put_counter))
			end

			last_put_counter = put_counter
			frame, grabbed = sm.buf_to_frame(buf, w, h)
		end
		time.sleep(0.001)
	end
end
