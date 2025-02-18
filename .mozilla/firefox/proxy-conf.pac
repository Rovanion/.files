// https://developer.mozilla.org/en-US/docs/Web/HTTP/Proxy_servers_and_tunneling/Proxy_Auto-Configuration_PAC_file
function FindProxyForURL(url, host) {
	const proxies = {
		"jump": "SOCKS5 localhost:8081",
		"berra": "SOCKS5 localhost:8083",
		"berra2": "SOCKS5 localhost:8087",
		"berra3": "SOCKS5 localhost:8087",
		"hype": "SOCKS5 localhost:8085",
		"berzelius0": "SOCKS5 localhost:8088",
		"ce1": "SOCKS5 localhost:8089",
	};

	const proxy_aliases = {
		"hypothesis1": "hype",
		"berra2-mgmt": "hype",
		"birdseye.nsc.liu.se": "jump",
		"pkgrepo.nsc.liu.se": "jump",
		"mail.nsc.liu.se": "jump",
		"hype": "hype",
		"192.168.100.2": "berzelius0",
		"10.126.27.254": "ce1",
	}

	if (proxies[proxy_aliases[host]]) {
		alert("Proxying with: " + proxies[proxy_aliases[host]])
	}

	if (host.startsWith("nodem")) {
		return proxies["berra2"]
	}
	if (host.endsWith("mgt.cluster")) {
		return proxies["berra"]
	}
	return proxies[proxy_aliases[host]] || proxies[host]
}
