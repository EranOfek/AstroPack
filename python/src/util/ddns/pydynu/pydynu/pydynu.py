# Copyright (C) 2019 Federico A. Corazza - All Rights Reserved
# You may use, distribute and modify this code under the
# terms described in the LICENSE document at the root of
# this project.

import json

import requests

from .utils import load_api_key, handle_response

# variables
api_key_path = "api.key"
endpoint = "https://api.dynu.com/v2"
headers = {
    "accept" : "application/json",
    "API-Key" : load_api_key(api_key_path)
}

# dns

@handle_response
def list_domains_for_dns_service():
    '''
    Get a list of domains for DNS service.
    '''
    return requests.get(f"{endpoint}/dns", headers=headers)

@handle_response
def add_dns_service(domain_name, group, ipv4Address, ipv6Address, ttl=120, ipv4=True, ipv6=True, ipv4WildcardAlias=True, ipv6WildcardAlias=True, allowZoneTransfer=False, dnssec=False):
    '''
    Add a new DNS service.
    '''
    payload = {
        "name": domain_name,
        "group": group,
        "ipv4Address": ipv4Address,
        "ipv6Address": ipv6Address,
        "ttl": ttl,
        "ipv4": ipv4,
        "ipv6": ipv6,
        "ipv4WildcardAlias": ipv4WildcardAlias,
        "ipv6WildcardAlias": ipv6WildcardAlias,
        "allowZoneTransfer": allowZoneTransfer,
        "dnssec": dnssec
    }
    return requests.post(f"{endpoint}/dns", headers=headers, data=json.dumps(payload))

@handle_response
def root_domain_name_by_hostname(hostname):
    '''
    Get the root domain name based on a hostname.
    '''
    return requests.get(f"{endpoint}/dns/getroot/{hostname}", headers=headers)

@handle_response
def dns_records_by_hostname(hostname, recordType):
    '''
    Get DNS records based on a hostname and resource record type.
    '''
    return requests.get(f"{endpoint}/dns/record/{hostname}?recordType={recordType}", headers=headers)

@handle_response
def domain_details_for_dns_service(id):
    '''
    Get details of a domain for DNS service.
    '''
    return requests.get(f"{endpoint}/dns/{id}", headers=headers)

@handle_response
def update_dns_service(id, domain_name, group, ipv4Address, ipv6Address, ttl=120, ipv4=True, ipv6=True, ipv4WildcardAlias=True, ipv6WildcardAlias=True, allowZoneTransfer=False, dnssec=False):
    '''
    Update an existing DNS service.
    '''
    payload = {
        "name": domain_name,
        "group": group,
        "ipv4Address": ipv4Address,
        "ipv6Address": ipv6Address,
        "ttl": ttl,
        "ipv4": ipv4,
        "ipv6": ipv6,
        "ipv4WildcardAlias": ipv4WildcardAlias,
        "ipv6WildcardAlias": ipv6WildcardAlias,
        "allowZoneTransfer": allowZoneTransfer,
        "dnssec": dnssec
    }
    return requests.post(f"{endpoint}/dns/{id}", headers=headers, data=json.dumps(payload))

@handle_response
def remove_domain_from_dns_service(id):
    '''
    Remove domain from DNS service.
    '''
    return requests.delete(f"{endpoint}/dns/{id}", headers=headers)

@handle_response
def ds_record(id):
    '''
    DS record of DNSSEC for DNS service.
    '''
    return requests.get(f"{endpoint}/dns/{id}/dnssec", headers=headers)

@handle_response
def enable_dnssec(id):
    '''
    Enable DNSSEC for DNS service.
    '''
    return requests.get(f"{endpoint}/dns/{id}/dnses/enable", headers=headers)

@handle_response
def disable_dnssec(id):
    '''
    Disable DNSSEC for DNS service.
    '''
    return requests.get(f"{endpoint}/dns/{id}/dnsec/disable", headers=headers)

@handle_response
def list_dns_records(id):
    '''
    Get a list of DNS records for DNS service.
    '''
    return requests.get(f"{endpoint}/dns/{id}/record", headers=headers)

@handle_response
def add_dns_record(id, nodeName, recordType, ttl, state, group, ipv4Address):
    '''
    Add a new DNS record for DNS service.
    '''
    payload = {
        "nodeName": nodeName,
        "recordType": recordType,
        "ttl": ttl,
        "state": state,
        "group": group,
        "ipv4Address": ipv4Address
    }
    return requests.post(f"{endpoint}/dns/{id}/record", headers=headers, data=json.dumps(payload))

@handle_response
def dns_record_details(id, dnsRecordId):
    '''
    Get details of a DNS record for DNS service.
    '''
    return requests.get(f"{endpoint}/dns/{id}/record/{dnsRecordId}", headers=headers)

@handle_response
def update_dns_record(id, dnsRecordId, nodeName, recordType, ttl, state, group, ipv4Address):
    '''
    Update an existing DNS record for DNS service.
    '''
    payload = {
        "nodeName": nodeName,
        "recordType": recordType,
        "ttl": ttl,
        "state": state,
        "group": group,
        "ipv4Address": ipv4Address
    }
    return requests.post(f"{endpoint}/dns/{id}/record/{dnsRecordId}", headers=headers, data=json.dumps(payload))

@handle_response
def remove_dns_record(id, dnsRecordId):
    '''
    Remove a DNS record from DNS service.
    '''
    return requests.delete(f"{endpoint}/dns/{id}/record/{dnsRecordId}", headers=headers)

@handle_response
def list_ip_update_history():
    '''
    Get a list of IP address updates.
    '''
    return requests.get(f"{endpoint}/dns/ipUpdateHistory", headers=headers)

@handle_response
def list_groups():
    '''
    Get a list of groups to which hosts are assigned to.
    '''
    return requests.get(f"{endpoint}/dns/group", headers=headers)

# domain

@handle_response
def list_domains():
    '''
    Get a list of domains for domain registration service.
    '''
    return requests.get(f"{endpoint}/domain", headers=headers)

@handle_response
def domain_details(id):
    '''
    Get details of a domain registration domain.
    '''
    return requests.get(f"{endpoint}/domain/{id}", headers=headers)

@handle_response
def enable_autorenewal(id):
    '''
    Enable autorenewal for a domain.
    '''
    return requests.get(f"{endpoint}/domain/{id}/autorenewEnable", headers=headers)

@handle_response
def disable_autorenewal(id):
    '''
    Disable autorenewal for a domain.
    '''
    return requests.get(f"{endpoint}/domain/{id}/autorenewDisable", headers=headers)

@handle_response
def lock_domain(id):
    '''
    Lock a domain.
    '''
    return requests.get(f"{endpoint}/domain/{id}/lock", headers=headers)

@handle_response
def unlock_domain(id):
    '''
    Unlock a domain.
    '''
    return requests.get(f"{endpoint}/domain/{id}/unlock", headers=headers)

@handle_response
def list_name_servers_by_domain(id):
    '''
    Get a list of name servers for a domain.
    '''
    return requests.get(f"{endpoint}/domain/{id}/nameServer", headers=headers)

@handle_response
def remove_name_server_of_domain(id, nameServer):
    '''
    Delete a name server of a domain.
    '''
    return requests.delete(f"{endpoint}/domain/{id}/nameServer?nameServer={nameServer}", headers=headers)

@handle_response
def add_name_server_to_domain(id, nameServer):
    '''
    Add a name server to a domain.
    '''
    return requests.get(f"{endpoint}/domain/{id}/nameServer/add?nameServer={nameServer}", headers=headers)

@handle_response
def make_name_server_primary_for_domain(id, nameServer):
    '''
    Make a name server the primary name server of a domain.
    '''
    return requests.get(f"{endpoint}/domain/{id}/nameServer/primary?nameServer={nameServer}", headers=headers)

@handle_response
def remove_domain(id):
    '''
    Cancel a domain.
    '''
    return requests.get(f"{endpoint}/domain/{id}/nameServer/cancel", headers=headers)

# email

@handle_response
def list_email_services():
    '''
    Get a list of email services.
    '''
    return requests.get(f"{endpoint}/email", headers=headers)

@handle_response
def email_service_details(id):
    '''
    Get details of an email service.
    '''
    return requests.get(f"{endpoint}/email/{id}", headers=headers)

@handle_response
def messages_in_delivery_queue(id):
    '''
    Get a list of messages in delivery queue.
    '''
    return requests.get(f"{endpoint}/email/{id}/deliveryQueue", headers=headers)

# monitor

@handle_response
def list_monitors():
    '''
    Get a list of monitors.
    '''
    return requests.get(f"{endpoint}/monitor", headers=headers)

@handle_response
def add_monitor(monitor_name, type, checkInterval, url, authenticationType="NONE"):
    '''
    Add a new monitor.
    '''
    payload = {
        "name": monitor_name,
        "type": type,
        "checkInterval": checkInterval,
        "url": url,
        "authenticationType": authenticationType
    }
    return requests.post(f"{endpoint}/monitor", headers=headers, data=json.dumps(payload))

@handle_response
def monitor_details(id):
    '''
    Get details of a monitor.
    '''
    return requests.get(f"{endpoint}/monitor/{id}", headers=headers)

@handle_response
def delete_monitor(id):
    '''
    Delete a monitor.
    '''
    return requests.delete(f"{endpoint}/monitor/{id}", headers=headers)

@handle_response
def pause_monitor(id):
    '''
    Pause a monitor.
    '''
    return requests.get(f"{endpoint}/monitor/{id}/pause", headers=headers)

@handle_response
def unpause_monitor(id):
    '''
    Unpause a monitor.
    '''
    return requests.get(f"{endpoint}/monitor/{id}/unpause", headers=headers)

# ping

@handle_response
def ping(request_type="GET", message="pong"):
    '''
    Ping the API server to obtain the pong response.
    '''
    if request_type == "GET":
        return requests.get(f"{endpoint}/ping?message={message}", headers=headers)
    else:
        payload = {
            "message": message
        }
        return requests.post(f"{endpoint}/ping", headers=headers, data=json.dumps(payload))
