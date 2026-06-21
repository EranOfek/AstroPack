import requests

BASE_URL = "http://127.0.0.1:8299"


def test_api(endpoint, a, b):
    """ Sends a POST request to the given endpoint with `a` and `b`. """
    url = f"{BASE_URL}{endpoint}"
    payload = {"a": a, "b": b}

    response = requests.post(url, json=payload)

    if response.status_code == 200:
        data = response.json()
        print(f"\nSuccess: {endpoint}")
        print(f"  ➝ Result: {data['result']} (Type: {type(data['result'])})")
        print(f"  ➝ Status: {data['status']} (Type: {type(data['status'])})")
        print(f"  ➝ Message: {data['message']} (Type: {type(data['message'])})")
    else:
        print(f"\nError: {endpoint} (HTTP {response.status_code})")
        print(response.text)


if __name__ == "__main__":
    print("Testing FastAPI Server...\n")

    # Test addition: 2 + 2
    test_api("/add", 2, 2)

    # Test multiplication: 2 * 2
    test_api("/multiply", 2, 2)
