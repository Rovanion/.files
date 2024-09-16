// https://developer.mozilla.org/en-US/docs/Web/HTTP/Proxy_servers_and_tunneling/Proxy_Auto-Configuration_PAC_file
function FindProxyForURL(url, host) {
    const proxy_definitions = {
        "jump": "SOCKS5 localhost:8081",
        "berra": "SOCKS5 localhost:8083",
        "berra2": "SOCKS5 localhost:8087",
        "hype": "SOCKS5 localhost:8085",
        "berzelius3": "SOCKS5 localhost:8088"
    };

    const proxies = {
        "berra1": "berra",
        "berra2": "berra2",
        "hypothesis1": "hype",
        "berra2-mgmt": "hype",
        "birdseye.nsc.liu.se": "jump",
        "pkgrepo.nsc.liu.se": "jump",
        "mail.nsc.liu.se": "jump",
        "hype": "hype",
        "192.168.100.2": "berzelius3",
    }

    //    var desired_proxy = proxies[host]
    //    if (desired_proxy) {
    //        var proxy_definition = proxy_definitions[desired_proxy]
    //        return proxy_definition
    //    }
    if (proxy_definitions[proxies[host]]) {
        alert("Proxying with: " + proxy_definitions[proxies[host]])
    }

    if (host.startsWith("nodem")) {
        return proxy_definitions["berra2"]
    }
    if (host.endsWith("mgt.cluster")) {
        return proxy_definitions["berra"]
    }
    return proxy_definitions[proxies[host]]
}
