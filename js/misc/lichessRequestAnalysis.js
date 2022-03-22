const ANALYZE_WAIT = 300 * 1000;
const DAILY_LIMIT_WAIT = 12 * 3600 * 1000;
const RATE_LIMIT = 5 * 1000;

const delay = (ms) => new Promise((res) => setTimeout(res, ms));

const requestAnalysis = (gameId) =>
  fetch(`https://lichess.org/${gameId}/request-analysis`, {
    cache: "no-cache",
    credentials: "same-origin", // required for safari < 12
    headers: {
      "X-Requested-With": "XMLHttpRequest", // so lila knows it"s XHR
    },
    method: "post",
  });

const parseGameIdFromUrl = (url) =>
  url.match(/^https?:\/\/lichess.org\/([a-zA-Z0-9]{8})\/?.*$/)[1];

const tryAnalyseGameId = async (gameId) => {
  console.log(`Requesting analysis for https://lichess.org/${gameId} ...`);
  const response = await requestAnalysis(gameId);
  const text = await response.text();
  console.log(response);
  console.log(text);

  if (text === "This game is already analysed") {
    await delay(RATE_LIMIT);
    return "success";
  }

  if (text === "You already have an ongoing requested analysis") {
    await delay(Math.max(RATE_LIMIT, ANALYZE_WAIT));
    return "try_again";
  }

  if (
    text === "You have reached the weekly analysis limit" ||
    text === "You have reached the daily analysis limit" ||
    text === "You have reached the daily analysis limit on this IP"
  ) {
    await delay(Math.max(RATE_LIMIT, DAILY_LIMIT_WAIT));
    return "try_again";
  }

  await delay(Math.max(RATE_LIMIT, ANALYZE_WAIT));
  return "success";
};

const requestAnalysisForUrls = async (gameUrls) => {
  for (let i = 0; i < gameUrls.length; i++) {
    const gameUrl = gameUrls[i];
    const gameId = parseGameIdFromUrl(gameUrl);

    let result = "try_again";
    while (result === "try_again") {
      result = await tryAnalyseGameId(gameId);
    }
  }
};

const main = async () => {
  const gameUrls = [
    "https://lichess.org/XlqrEfcT",
    "https://lichess.org/UBUPq5Rc",
    "https://lichess.org/FJkeS547",
    "https://lichess.org/3WYsEfVe",
    // ...
  ];

  await requestAnalysisForUrls(gameUrls);
};

await main();
