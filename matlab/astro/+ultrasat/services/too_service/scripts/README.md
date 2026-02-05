#


https://chatgpt.com/c/69073849-2afc-832b-b1f7-6be306bda218


/etc/systemd/system/soc-too-matlab.service

/etc/systemd/system/soc-too-service.service


## Enable and Control

	sudo systemctl daemon-reload
	sudo systemctl enable soc-too-matlab.service
	sudo systemctl enable soc-too-service.service
	sudo systemctl start soc-too-matlab.service
	sudo systemctl start soc-too-service.service


## Check status/logs anytime:

	systemctl status soc-too-matlab
	journalctl -u soc-too-service -f

