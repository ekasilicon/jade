'reach 0.1';
'use strict';

export const main = Reach.App(() => {
  const A = Participant('Alice', {
    getSwap: Fun([], Tuple(Token, UInt, Token, UInt, UInt)),
  });
  const B = Participant('Bob', {
    accSwap: Fun([Token, UInt, Token, UInt], Bool),
  });
  deploy();

  A.only(() => {
    const [ tokenA, amtA, tokenB, amtB, time ] = declassify(interact.getSwap());
    assume(tokenA != tokenB); });
  A.publish(tokenA, amtA, tokenB, amtB, time)
    .pay([ [amtA, tokenA] ]);
  commit();

  B.only(() => {
    const bwhen = declassify(interact.accSwap(tokenA, amtA, tokenB, amtB)); });
  B.pay([ [amtB, tokenB] ])
    .when(bwhen)
    .timeout(time, () => {
      A.publish();
      transfer(amtA, tokenA).to(A);
      commit();
      exit();
    });
  transfer(amtB, tokenB).to(A);
  transfer([ [amtA, tokenA] ]).to(B);
  commit();

  exit();
});
