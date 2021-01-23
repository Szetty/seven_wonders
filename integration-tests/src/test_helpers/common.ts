import { time } from "console";
import { Username } from "./api";
import { Client } from "./client";

export async function delay(ms: number) {
  return new Promise(resolve => setTimeout(resolve, ms));
}

export async function waitFor(f: () => Promise<any>, timeout: number = 5000, step: number = 500): Promise<any> {
  let current = 0;
  while (current <= timeout) {
    try {
      return await f();
    } catch (_) {
      current += step;
    }
  }
  throw new Error("waitFor timeout");
}

export async function expectNotification(expect: () => Promise<any>, errorMsg: string): Promise<any> {
  try {
    return await waitFor(expect);
  } catch {
    throw new Error(`Expected to ${errorMsg}`)
  }
}

export async function expectOnlineUsers(c: Client, online_usernames: Username[]) {
  const res = await c.getOnlineUsers();
  online_usernames.forEach(name => expect(res).toContainEqual(name));
}