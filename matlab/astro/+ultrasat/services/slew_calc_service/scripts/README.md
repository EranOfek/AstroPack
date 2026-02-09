#


https://chatgpt.com/c/69073849-2afc-832b-b1f7-6be306bda218


/etc/systemd/system/soc-slew-calc-matlab.service

/etc/systemd/system/soc-slew-calc-service.service


## Enable and Control

	sudo systemctl daemon-reload
	sudo systemctl enable soc-slew-calc-matlab.service
	sudo systemctl enable soc-slew-calc-service.service
	sudo systemctl start soc-slew-calc-matlab.service
	sudo systemctl start soc-slew-calc-service.service


## Check status/logs anytime:

	systemctl status soc-slew-calc-matlab
	journalctl -u soc-slew-calc-service -f

