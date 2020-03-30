var trackingCookieName = "enableTracking";

function handleTracking(){
	let trackingCookie = getCookie(trackingCookieName);
	if(trackingCookie === null){
		openCookieBar();	
	}

	if(trackingCookie){
		startTracking();
	}
}

function openCookieBar(){
	let cookieBarClasslist = document.getElementById("cookie-overlay").classList;
	if(cookieBarClasslist.contains("hidden")){
		cookieBarClasslist.remove("hidden");
	}
}

async function closeCookieBar(){
	let cookieBar = document.getElementById("cookie-overlay");
	cookieBar.classList.add("hidden");
}

async function acceptCookies(){
	closeCookieBar();
	setCookie(trackingCookieName, true, 365);
	startTracking();
}

async function declineCookies(){
	closeCookieBar();
	setCookie(trackingCookieName, false, 365);
}


function setCookie(name,value,days) {
    var expires = "";
    if (days) {
        var date = new Date();
        date.setTime(date.getTime() + (days*24*60*60*1000));
        expires = "; expires=" + date.toUTCString();
    }
    document.cookie = name + "=" + (value || "")  + expires + "; path=/; secure";
}
function getCookie(name) {
    var nameEQ = name + "=";
    var ca = document.cookie.split(';');
    for(var i=0;i < ca.length;i++) {
        var c = ca[i];
        while (c.charAt(0)==' ') c = c.substring(1,c.length);
        if (c.indexOf(nameEQ) == 0) return c.substring(nameEQ.length,c.length);
    }
    return null;
}

function startTracking(){
	  (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
		(i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
		m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
		})(window,document,'script','https://www.google-analytics.com/analytics.js','ga');

		ga('create', 'UA-43715401-3', 'auto');
		ga('send', 'pageview');
}
