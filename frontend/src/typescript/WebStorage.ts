export function init_flags(): string {
    let userInfo = JSON.parse(get('userInfo'));
    let notifications = JSON.parse(get('notifications')) || [];
    if (userInfo) {
        return JSON.stringify({ userInfo, notifications } as SessionData)
    } else {
        return ""
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

type SessionData =
    { userInfo : string
    , notifications : string
    }

type StoreKey = 'notifications' | 'userInfo';

function store(key: StoreKey, value: string) {
    console.log(`Storing ${key}`, value);
    localStorage.setItem(key, value);
}

function get(key: StoreKey): string {
    return localStorage.getItem(key);
}

