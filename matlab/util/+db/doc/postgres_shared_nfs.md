## Ubuntu NFS Setup - @TODO

NOTE: This section is not completed yet, still need some experiments and testing.
	
Currently we use SAMBA which might be little bit slower? (according to some web sites).


### NFS vs SAMBA

    NFS offers better performance and is unbeatable if the files are medium-sized or small.
    For larger files, the timings of both methods are almost the same.
    In the case of sequential read, the performance of NFS and SMB are almost
    the same when using plain text.


## NFS Installation

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



### On Client


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



