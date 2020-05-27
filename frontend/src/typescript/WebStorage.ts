export interface Flags {
    userInfo?: UserInfo
}

export interface UserInfo {
    name: string
    userToken: string
    gameID: string
}

type StoreKey = 'userInfo';

export function init_flags(): Flags {
    let userInfo: UserInfo = JSON.parse(get('userInfo'));
    return {
        userInfo
    }
}

export function storeUserInfo(userInfo) {
    store('userInfo', userInfo);
}

export function deleteItem(key) {
    localStorage.removeItem(key);
}

function store(key: StoreKey, value: string) {
    console.log(`Storing ${key}`, value);
    localStorage.setItem(key, value);
}

function get(key: StoreKey): string {
    return localStorage.getItem(key);
}

