module.exports = {
  preset: 'ts-jest',
  testEnvironment: 'node',
  rootDir: '.',
  testRegex: '.*\\.test.(js|jsx|ts|tsx)$',
  setupFilesAfterEnv: ['<rootDir>/jest-setup.js'],
  transform: {
    '\\.ts$': 'ts-jest',
    '\\.js$': 'ts-jest',
  },
  transformIgnorePatterns: [],
  testTimeout: 60000
};