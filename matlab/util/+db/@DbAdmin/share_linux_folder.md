# Share Linux Folder

Serevr host name: gauss


## Server - Shared Linux Folder using SAMBA

[https://www.howtogeek.com/176471/how-to-share-files-between-windows-and-linux/](https://www.howtogeek.com/176471/how-to-share-files-between-windows-and-linux/)


On server:

    sudo apt-get install samba
    smbpasswd -a root
    pass
    pass

    sudo mkdir /var/samba/pgshare
    sudo chmod 777 /var/samba/pgshare
	
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
    #valid users = odroid


Type

    sudo service smbd restart


## Client -

Install and create mount folder

    sudo apt-get install cifs-utils

    sudo mkdir /media/gauss_pgshare
    sudo chown -R nobody:nogroup /media/gauss_pgshare
    sudo chmod -R 0777 /media/gauss_pgshare
    sudo nano /etc/fstab


### Mount on Linux as guest (no user/password is required, grant full access to folder)

    sudo nano /etc/fstab

    //gauss/pgshare /media/gauss_pgshare cifs rw,guest,uid=nobody,iocharset=utf8,file_mode=0777,dir_mode=0777,noperm 0 0


Type:

    sudo mount -a



### Client - Windows - Mount Shared SAMBA Folder on Windows

    net use S: \\gauss\pgshare





## NFS - @TODO


NFS vs SAMBA

    NFS offers better performance and is unbeatable if the files are medium-sized or small.
    For larger files, the timings of both methods are almost the same.
    In the case of sequential read, the performance of NFS and SMB are almost
    the same when using plain text.



https://www.tecmint.com/install-nfs-server-on-ubuntu/





https://www.digitalocean.com/community/tutorials/how-to-set-up-an-nfs-mount-on-ubuntu-18-04


On the Host

    sudo apt update
    sudo apt install nfs-kernel-server


    sudo mkdir /var/nfs/pgshare -p
    sudo chown nobody:nogroup /var/nfs/pgshare

    sudo nano /etc/exports


    /var/nfs/pgshare *(rw,sync,no_subtree_check)

    sudo systemctl restart nfs-kernel-server



Creating Mount Points and Mounting Directories on the Client

    sudo mkdir -p /nfs/gauss_pgshare

    sudo mount gauss:/var/nfs/pgshare /nfs/gauss_pgshare



On the Client

    sudo apt update
    sudo apt install nfs-common



On server:


    sudo mkdir /pgshare
    sudo chmod a+rw /pgshare
    sudo chown chent pgshare



On client:


1. Create a directory to serve as the mount point for the remote filesystem:

    cd /media
    sudo mkdir gauss_share
    sudo chmod a+rw gauss_share



2. Generally, you will want to mount the remote NFS share automatically at boot.
   To do so open the /etc/fstab file with your text editor:

    sudo nano /etc/fstab



Add

    # <file system>    <dir>       <type>   <options>   <dump>  <pass>

    remote.server:/dir /media/gauss_share  nfs      defaults    0       0


3. Mount the NFS share by running the following command:

    sudo mount /media/gauss_share


https://linuxize.com/post/how-to-mount-and-unmount-file-systems-in-linux/



