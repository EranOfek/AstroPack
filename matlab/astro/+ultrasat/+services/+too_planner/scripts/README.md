#


https://chatgpt.com/c/69073849-2afc-832b-b1f7-6be306bda218


/etc/systemd/system/soc-too-matlab.service

/etc/systemd/system/soc-too-service.service


## Enable and Control

	sudo systemctl daemon-reload
	sudo systemctl enable soc-too-planner-matlab.service
	sudo systemctl enable soc-too-planner-service.service
	sudo systemctl start soc-too-planner-matlab.service
	sudo systemctl start soc-too-planner-service.service


## Check status/logs anytime:

	systemctl status soc-too-planner-matlab
	journalctl -u soc-too-planner-service -f


## Log Folder

Ensure the directory exists and is writable by the service user:

	sudo mkdir -p /var/log/ultrasat/soc/matlab/too_planner
	sudo chown -R soc:soc /var/log/ultrasat/soc/matlab/too_planner

