// -*- mode: js-mode -*-
// https://developer.mozilla.org/en-US/docs/Web/HTTP/Proxy_servers_and_tunneling/Proxy_Auto-Configuration_PAC_file
function FindProxyForURL(url, host) {
	const proxies = {
		"login": "SOCKS5 localhost:8081",
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
		"birdseye.nsc.liu.se": "login",
		"pkgrepo.nsc.liu.se": "login",
		"mail.nsc.liu.se": "login",
		"hype": "hype",
		"192.168.100.2": "berzelius0",
		"10.126.27.254": "ce1",
		"monitoring": "ce1",
		"vast.mgt": "berra2",
		"s1": "berra2",
	}

	if (proxies[proxy_aliases[host]]) {
		alert("Proxying with: " + proxies[proxy_aliases[host]])
	}

	if (host.startsWith("nodem0") || host.startsWith("node0")) {
		return proxies["berra2"]
	}
	if (host.startsWith("nodem1") || host.startsWith("node1")) {
		return proxies["ce1"]
	}
	if (host.endsWith("mgt.cluster")) {
		return proxies["berra"]
	}
	if (host.startsWith("poppius") && host.endsWith("-management")) {
		return proxies["ce1"]
	}

	return proxies[proxy_aliases[host]] || proxies[host]
}
