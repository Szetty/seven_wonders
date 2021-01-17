import { Client, UserContext } from "./client";
const axios = require('axios');

const SERVER_URL = "localhost:8080";
const ACCESS_TOKEN = "7";

export async function init_client(path: string, userContext: UserContext = undefined): Promise<Client> {
    if (!userContext) {
        userContext = await getUserContext();
    }
    const url = `${SERVER_URL}${path}`;
    return new Promise(resolve => {
        const client = new Client(
            url,
            userContext,
            (data) => {
                expect(data.type).toBe('Welcome');
                resolve(client); 
            }
        );
    })
}

export async function getUserContext(name = randomName()): Promise<UserContext> {
    const url = `http://${SERVER_URL}/api/login`;
    const payload = {
        access_token: ACCESS_TOKEN,
        name
    }
    const {user_token: userToken, game_id: gameID} = (await axios.post(url, payload)).data;
    return {userToken, name, gameID}
}

function randomName(): string {
    return `user_${new Date().valueOf()}`;
}