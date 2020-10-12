pragma solidity ^0.5.17;

contract PayableTest {
   
    event Content(uint _paidContent);
  
    function seeContent() payable public returns(uint){
        if (msg.value == 1000000000000000000) {
            emit Content(1);
        } else {
            emit Content(0);
        }
    }
}
