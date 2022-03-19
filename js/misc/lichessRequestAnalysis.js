const RATE_LIMIT = 1200 * 1000;

const delay = ms => new Promise(res => setTimeout(res, ms));

const requestAnalysis = gameId => fetch(
  `https://lichess.org/${gameId}/request-analysis`, {
    cache: "no-cache",
    credentials: "same-origin", // required for safari < 12
    headers: {
      "X-Requested-With": "XMLHttpRequest", // so lila knows it"s XHR
    },
    method: "post",
  }
).then(response => {
  const text = response.text();
  console.log(text);
});

const parseGameIdFromUrl = url => (
  url.match(/^https?:\/\/lichess.org\/([a-zA-Z0-9]+)\/?.*$/)[1]
);

const requestAnalysisForUrls = async gameUrls => {
  for (let i = 0; i < gameUrls.length; i++) {
    const gameUrl = gameUrls[i];
    const gameId = parseGameIdFromUrl(gameUrl);
    console.log(`Requesting analysis for ${gameId}...`);
    await requestAnalysis(gameId);
    await delay(RATE_LIMIT);
  }
}

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
