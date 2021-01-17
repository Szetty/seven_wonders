export function init_flags() {
    let userInfo = JSON.parse(get('userInfo'));
    let notifications = JSON.parse(get('notifications')) || [];
    if (userInfo) {
        return JSON.stringify({ userInfo, notifications });
    } else {
        return "";
    }
}

export function storeUserInfo(userInfo) {
    store('userInfo', userInfo);
}

export function storeNotifications(notifications) {
    store('notifications', notifications);
}

export function clearStorage() {
    localStorage.clear();
}

function store(key, value) {
    console.log(`Storing ${key}`, value);
    localStorage.setItem(key, value);
}

function get(key) {
    return localStorage.getItem(key);
}