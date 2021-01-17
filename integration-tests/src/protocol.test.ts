import { init_client } from "./test_helpers/init";

test('Start', async () => {
  const c = await init_client('/api/secured/game/lobby');
  c.close();
});