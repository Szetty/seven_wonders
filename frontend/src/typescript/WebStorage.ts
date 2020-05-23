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
    let userInfo = get('userInfo');
    return {
        userInfo: JSON.parse(userInfo)
    }
}

export function storeUserInfo(userInfo) {
    store('userInfo', userInfo);
}

function store(key: StoreKey, value: string) {
    console.log(`Storing ${key}`, value);
    localStorage.setItem(key, value);
}

function get(key: StoreKey): string {
    return localStorage.getItem(key);
}
