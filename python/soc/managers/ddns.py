# Copyright (C) 2019 Federico A. Corazza - All Rights Reserved
# You may use, distribute and modify this code under the
# terms described in the LICENSE document at the root of
# this project.

import logging

import requests

from pydynu import pydynu

format = "[%(asctime)s] %(levelname)s: %(message)s"
logging.basicConfig(format=format, level=logging.INFO)

r = requests.get("https://api.ipify.org")
ipv4Address = r.text
logging.info(ipv4Address)

domains = pydynu.list_domains_for_dns_service()
logging.info(domains)

for domain in domains["domains"]:
    logging.info(pydynu.update_dns_service(domain["id"], domain["name"], "", ipv4Address, None, ttl=300))

domains = pydynu.list_domains_for_dns_service()
logging.info(domains)
