# Share Linux Folder

Use the following instructions to create a shared folder between Linux server running PostgreSQL, 
and a client (Linux/Windows).

It is required in order to use Postgre's COPY TO/FROM functionality for high performance INSERT/SELECT.


## Server - Linux - Share Folder using SAMBA

Serevr host name: gauss

    ServerSharePath : '/var/samba/pgshare'
    MountSharePath  : '/media/gauss_pgshare'
	
Reference:
	
[https://www.howtogeek.com/176471/how-to-share-files-between-windows-and-linux/](https://www.howtogeek.com/176471/how-to-share-files-between-windows-and-linux/)


On **server** machine:

    sudo apt-get install samba
    smbpasswd -a root
    pass
    pass

    sudo mkdir /var/samba/pgshare
    sudo chmod 777 /var/samba/pgshare

Edit /etc/samba/smb.conf

    sudo nano /etc/samba/smb.conf


Add at the end of smb.conf

	[pgshare]
	path = /var/samba/pgshare
	available = yes
	read only = yes
	browsable = yes
	public = yes
	writable = yes
	guest ok = yes


Restart the service.

    sudo service smbd restart


### Client - Linux - Mount Shared SAMBA Folder

On **client** machine:

Mount on Linux as guest.
No user/password is required, grant full access to folder.

Install cifs and create folder with correct permissions:

    sudo apt-get install cifs-utils

For host 'gauss' (Eran)

    sudo mkdir /media/gauss_pgshare
    sudo chown -R nobody:nogroup /media/gauss_pgshare
    sudo chmod -R 0777 /media/gauss_pgshare
    sudo nano /etc/fstab

For host 'scorpius' (Yossi)

    sudo mkdir /media/scorpius_pgshare
    sudo chown -R nobody:nogroup /media/scorpius_pgshare
    sudo chmod -R 0777 /media/scorpius_pgshare
    sudo nano /etc/fstab



Edit /etc/fstab:

    sudo nano /etc/fstab
    //gauss/pgshare /media/gauss_pgshare cifs rw,guest,uid=nobody,iocharset=utf8,file_mode=0777,dir_mode=0777,noperm 0 0
    //scorpius/pgshare /media/scorpius_pgshare cifs rw,guest,uid=nobody,iocharset=utf8,file_mode=0777,dir_mode=0777,noperm 0 0


After saving the modifications, reload mount table:

    sudo mount -a

Test client access

    ls -la /media/gauss_pgshare/
    ls -la /media/scorpius_pgshare/

Test server access

    ls -la /var/samba/pgshare/


### Client - Windows - Mount Shared SAMBA Folder

Open **cmd** window and type:

    net use S: \\gauss\pgshare

