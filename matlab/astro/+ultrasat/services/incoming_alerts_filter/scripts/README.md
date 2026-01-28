#


https://chatgpt.com/c/69073849-2afc-832b-b1f7-6be306bda218


/etc/systemd/system/soc-slew-matlab.service

/etc/systemd/system/soc-slew-service.service


## Enable and Control

	sudo systemctl daemon-reload
	sudo systemctl enable soc-slew-matlab.service
	sudo systemctl enable soc-slew-service.service
	sudo systemctl start soc-slew-matlab.service
	sudo systemctl start soc-slew-service.service


## Check status/logs anytime:

	systemctl status soc-slew-matlab
	journalctl -u soc-slew-service -f

