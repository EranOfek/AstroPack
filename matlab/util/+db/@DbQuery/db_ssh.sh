#!/bin/bash

# This script is set up to accept command-line arguments in this order: 
# Host, Port, RemoteHost, RemotePort, User, Password. 
# If any argument is not provided, it will use the default value.
# Please replace the 'Cmd' command with the real one. The current command is a placeholder
# since providing passwords directly in command line is generally insecure. 
# Consider using key-based authentication or sshpass if you must automate the password entry. 
# However, note that these approaches have their own security implications.
# Also note that Ctrl+C will stop the script and close the SSH connection.
# Usage: ./my_script.sh [Host] [Port] [RemoteHost] [RemotePort] [User] [Password]

# Check number of arguments and set defaults
Host=${1:-localhost}
Port=${2:-63331}
RemoteHost=${3:-10.23.1.25}
RemotePort=${4:-5432}
User=${5:-ocs}
Password=${6:-physics}

while true
do
  echo "Establishing SSH connection..."

  # Here we build the SSH command
  Cmd="ssh -L $Port:$Host:$RemotePort $User@$RemoteHost"

  # Print and execute the command
  echo $Cmd
  $Cmd

  # If the SSH connection closed, we go back to Loop
  echo "SSH connection closed, retrying in 5 seconds..."
  sleep 5
done
