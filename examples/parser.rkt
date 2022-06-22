#lang racket/base
(require racket/pretty
         "../src/parse.rkt")

(define postmodern-assembly
  (parse
   #<<ASSEMBLY
#pragma version 6
	txn OnCompletion
	int 0
	==
	bnz label1
	txn OnCompletion
	intc 4 
	==
	bnz label2
	err
label2:
	global GroupSize
	int 1
	==
	txn Sender
	global CreatorAddress
	==
	txn Sender
	byte base64 dHJlYXN1cnk=
	app_global_get
	==
	||
	&&
	assert
	b label3
label1:
	txna ApplicationArgs 0
	pushbytes 0x7072696365 
	==
	bnz label4
	global GroupSize
	int 1
	==
	bnz label5
	global GroupSize
	int 2
	==
	bnz label6
	err
label6:
	global GroupSize
	int 2
	==
	gtxn 0 TypeEnum
	int 1
	==
	&&
	gtxn 0 Receiver
	global CurrentApplicationAddress
	==
	&&
	gtxn 0 Amount
	pushint 2000000
	>=
	&&
	gtxn 1 TypeEnum
	intc 5 
	==
	&&
	gtxn 1 ApplicationID
	global CurrentApplicationID
	==
	&&
	gtxn 1 Sender
	gtxn 0 Sender
	==
	&&
	gtxn 1 NumAppArgs
	int 1
	==
	&&
	gtxna 1 ApplicationArgs 0
	btoi
	int 1000000
	>=
	&&
	gtxna 1 ApplicationArgs 0
	btoi
	pushint 1000000000000
	<=
	&&
	assert
	gtxn 0 Amount
	int 1000000
	-
	store 0
	load 0
	byte base64 ZXVy
	app_global_get
	*
	int 1000000
	/
	store 1
	gtxna 1 ApplicationArgs 0
	btoi
	store 2
	load 1
	load 2
	pushint 100
	/
	pushint 151
	*
	>=
	assert
	load 0
	load 2
	callsub label7
	itxn CreatedApplicationID
	app_params_get AppAddress
	store 4
	store 3
	load 3
	load 0
	int 0
	int 0
	callsub label8
	gtxn 0 Sender
	load 2
	intc 6 
	int 0
	callsub label8
	b label3
label5:
	global GroupSize
	int 1
	==
	txn Sender
	global CreatorAddress
	==
	txn Sender
	byte base64 dHJlYXN1cnk=
	app_global_get
	==
	||
	&&
	assert
	txna ApplicationArgs 0
	byte base64 dmVyc2lvbg==
	==
	bnz label9
	txna ApplicationArgs 0
	bytec 4 
	==
	bnz label10
	txna ApplicationArgs 0
	byte base64 aW50ZXJlc3Q=
	==
	bnz label11
	txna ApplicationArgs 0
	byte base64 dHJlYXN1cnk=
	==
	bnz label12
	err
label12:
	byte base64 dHJlYXN1cnk=
	txna Accounts 1
	app_global_put
	b label3
label11:
	byte base64 aW50ZXJlc3Q=
	txna ApplicationArgs 1
	btoi
	app_global_put
	b label3
label10:
	bytec 4 
	txna ApplicationArgs 1
	btoi
	app_global_put
	b label3
label9:
	byte base64 dmVyc2lvbg==
	txna ApplicationArgs 1
	btoi
	app_global_put
	b label3
label4:
	global GroupSize
	int 1
	==
	txn Sender
	pushbytes 0x666565646572 
	app_global_get
	==
	&&
	txn NumAppArgs
	int 2
	==
	&&
	assert
	byte base64 ZXVy
	txna ApplicationArgs 1
	btoi
	app_global_put
	global CurrentApplicationAddress
	balance
	global CurrentApplicationAddress
	min_balance
	-
	pushint 20000000
	>
	txn Sender
	balance
	intc 7 
	<
	&&
	bnz label13
label14:
	global CurrentApplicationAddress
	balance
	global CurrentApplicationAddress
	min_balance
	-
	pushint 40000000
	>
	bz label3
	byte base64 dHJlYXN1cnk=
	app_global_get
	global CurrentApplicationAddress
	balance
	global CurrentApplicationAddress
	min_balance
	-
	int 1000000
	-
	int 0
	int 0
	callsub label8
	b label3
label13:
	txn Sender
	intc 7 
	int 0
	int 0
	callsub label8
	b label14
label3:
	int 1
	return
label8:
	store 8
	store 7
	store 6
	store 5
	itxn_begin
	load 7
	int 0
	==
	bnz label15
	intc 4 
	itxn_field TypeEnum
	load 5
	itxn_field AssetReceiver
	load 6
	itxn_field AssetAmount
	load 7
	itxn_field XferAsset
	load 8
	int 1
	==
	bz label16
	load 5
	itxn_field AssetCloseTo
	b label16
label15:
	int 1
	itxn_field TypeEnum
	load 5
	itxn_field Receiver
	load 6
	itxn_field Amount
	load 8
	int 1
	==
	bz label16
	load 5
	itxn_field CloseRemainderTo
label16:
	itxn_submit
	retsub
label7:
	store 10
	store 9
	itxn_begin
	intc 5 
	itxn_field TypeEnum
	int 0
	itxn_field OnCompletion
	intc 8 
	itxn_field GlobalNumUint
	intc 8 
	itxn_field GlobalNumByteSlice
	pushbytes 0x0620080001dad3dad102a08d06c0843d6402042606066973737565640763726561746f72036575720561737365740974696d657374616d7008696e746572657374311822124000dd3119810512400001002480087472656173757279653505350432042106123300102107121033001432091210330012286412103300112b6412103301108106121033011832081210330100330000121033011b22121044320a608801010f400073310029641240002c88012c2508320a600d400015310088011e2222880099340422222388009142008c31002222238800864200818800dc2508320a600d4000288800d0250c40001534048800c62222880065310022222388005d420058310022222388005242004d340422222388004742004231002964124442ff84270432076729361a006780076465706f736974361a01176728361a0217672705361a031767800776657273696f6e361a0417672b361a05176723433503350235013500b13402221240001e2107b2103400b2143401b2123402b2113403231241001d3400b21542001623b2103400b2073401b208340323124100043400b209b389242a6535093508286421060a81030b21040b34080a89242a6535093508286421050a2705640b3207270464090b81e08f860f0a21040b34080a892480067265776172646535073506242a6535093508286421050a21053406080b21040b34080a89 
	itxn_field ApprovalProgram
	pushbytes 0x06810143 
	itxn_field ClearStateProgram
	gtxn 0 Sender
	itxn_field ApplicationArgs
	load 9
	itob
	itxn_field ApplicationArgs
	load 10
	itob
	itxn_field ApplicationArgs
	byte base64 aW50ZXJlc3Q=
	app_global_get
	itob
	itxn_field ApplicationArgs
	byte base64 dmVyc2lvbg==
	app_global_get
	itob
	itxn_field ApplicationArgs
	intc 6 
	itob
	itxn_field ApplicationArgs
	itxn_submit
	retsub
ASSEMBLY
   ))

(define board-assembly
  (parse
   #<<ASSEMBLY
	#pragma version 5
	txn ApplicationID
	int 0
	==
	bnz label1
	txn OnCompletion
	pushint 5
	==
	bnz label2
	txn OnCompletion
	int 1
	==
	bnz label3
	txn OnCompletion
	int 2
	==
	bnz label4
	bytec 15 
	app_global_get
	int 1
	==
	bnz label5
	txna ApplicationArgs 0
	pushbytes 0x6970 
	==
	bnz label6
	err
label6:
	txn OnCompletion
	int 0
	==
	assert
	txn TypeEnum
	int 6
	==
	assert
	bytec 15 
	app_global_get
	!
	assert
	byte base64 YTE=
	app_global_get
	int 1
	>
	bnz label7
label14:
	byte base64 YTI=
	app_global_get
	int 1
	>
	bnz label8
label13:
	byte base64 YTE=
	app_global_get
	int 1
	==
	bnz label9
	byte base64 YTE=
	app_global_get
	asset_params_get AssetUnitName
	store 39
	store 40
	byte base64 YTI=
	app_global_get
	asset_params_get AssetUnitName
	store 41
	store 42
	load 39
	assert
	load 41
	assert
	bytec 26 
	load 40
	concat
	bytec 27 
	concat
	load 42
	concat
	bytec 28 
	concat
	store 27
label11:
	itxn_begin
	pushint 3
	itxn_field TypeEnum
	load 27
	itxn_field ConfigAssetName
	pushbytes 0x41462d504f4f4c 
	itxn_field ConfigAssetUnitName
	intc 8 
	itxn_field ConfigAssetTotal
	int 6
	itxn_field ConfigAssetDecimals
	pushbytes 0x68747470733a2f2f616c676f66692e6f7267 
	itxn_field ConfigAssetURL
	global CurrentApplicationAddress
	itxn_field ConfigAssetManager
	global CurrentApplicationAddress
	itxn_field ConfigAssetReserve
	itxn_submit
	bytec 16 
	itxn CreatedAssetID
	app_global_put
	intc 5 
	bytec 5 
	app_global_get_ex
	store 31
	store 32
	load 31
	assert
	load 32
	intc 4 
	<=
	assert
	bytec 5 
	load 32
	app_global_put
	intc 5 
	bytec 10 
	app_global_get_ex
	store 35
	store 36
	load 35
	assert
	load 36
	intc 4 
	<=
	assert
	bytec 10 
	load 36
	app_global_put
	intc 5 
	bytec 8 
	app_global_get_ex
	store 37
	store 38
	load 37
	assert
	load 38
	intc 4 
	<=
	assert
	bytec 8 
	load 38
	app_global_put
	bytec 11 
	int 0
	app_global_put
	intc 5 
	bytec 12 
	app_global_get_ex
	store 33
	store 34
	load 33
	assert
	bytec 12 
	load 34
	app_global_put
	bytec 17 
	global LatestTimestamp
	app_global_put
	byte base64 YjE=
	int 0
	app_global_put
	byte base64 YjI=
	int 0
	app_global_put
	bytec 4 
	int 0
	app_global_put
	bytec 6 
	int 0
	app_global_put
	bytec 7 
	int 0
	app_global_put
	bytec 19 
	int 0
	app_global_put
	bytec 20 
	int 0
	app_global_put
	bytec 21 
	int 0
	app_global_put
	bytec 22 
	int 0
	app_global_put
	bytec 23 
	int 0
	app_global_put
	bytec 24 
	int 0
	app_global_put
	bytec 13 
	int 0
	app_global_put
	bytec 14 
	int 0
	app_global_put
	pushbytes 0x6d61 
	intc 5 
	app_global_put
	pushbytes 0x736670 
	intc 10 
	app_global_put
	bytec 15 
	int 1
	app_global_put
	int 1
	b label10
label9:
	byte base64 YTI=
	app_global_get
	asset_params_get AssetUnitName
	store 41
	store 42
	load 41
	assert
	bytec 26 
	pushbytes 0x414c474f 
	concat
	bytec 27 
	concat
	load 42
	concat
	bytec 28 
	concat
	store 27
	b label11
label8:
	byte base64 YTI=
	app_global_get
	callsub label12
	b label13
label7:
	byte base64 YTE=
	app_global_get
	callsub label12
	b label14
label5:
	txn Sender
	bytec 9 
	app_global_get
	==
	bnz label15
	txn OnCompletion
	int 0
	==
	txn TypeEnum
	int 6
	==
	&&
	bnz label16
	err
label16:
	txna ApplicationArgs 0
	pushbytes 0x64756d6d79 
	==
	bnz label17
	txna ApplicationArgs 0
	bytec 25 
	==
	bnz label18
	txna ApplicationArgs 0
	bytec 29 
	==
	bnz label19
	txna ApplicationArgs 0
	bytec 30 
	==
	bnz label20
	txna ApplicationArgs 0
	bytec 31 
	==
	bnz label21
	txna ApplicationArgs 0
	bytec 32 
	==
	bnz label22
	txna ApplicationArgs 0
	bytec 18 
	==
	txna ApplicationArgs 0
	pushbytes 0x736566 
	==
	||
	bnz label23
	txna ApplicationArgs 0
	bytec 33 
	==
	bnz label24
	txna ApplicationArgs 0
	pushbytes 0x666c 
	==
	bnz label25
	err
label25:
	intc 5 
	bytec 5 
	app_global_get_ex
	store 31
	store 32
	load 31
	assert
	load 32
	intc 4 
	<=
	assert
	bytec 5 
	load 32
	app_global_put
	intc 5 
	bytec 10 
	app_global_get_ex
	store 35
	store 36
	load 35
	assert
	load 36
	intc 4 
	<=
	assert
	bytec 10 
	load 36
	app_global_put
	intc 5 
	bytec 8 
	app_global_get_ex
	store 37
	store 38
	load 37
	assert
	load 38
	intc 4 
	<=
	assert
	bytec 8 
	load 38
	app_global_put
	txna ApplicationArgs 2
	btoi
	bytec 10 
	app_global_get
	mulw
	int 0
	intc 4 
	divmodw
	pop
	pop
	swap
	!
	assert
	int 1
	+
	store 22
	txn OnCompletion
	int 0
	==
	assert
	txn TypeEnum
	int 6
	==
	assert
	txn GroupIndex
	int 0
	==
	assert
	txna ApplicationArgs 1
	btoi
	byte base64 YTE=
	app_global_get
	==
	txna ApplicationArgs 1
	btoi
	byte base64 YTI=
	app_global_get
	==
	||
	assert
	txna ApplicationArgs 2
	btoi
	int 0
	>
	assert
	txna ApplicationArgs 1
	btoi
	byte base64 YTE=
	app_global_get
	==
	bnz label26
	txna ApplicationArgs 2
	btoi
	byte base64 YjI=
	app_global_get
	bytec 8 
	app_global_get
	mulw
	int 0
	intc 4 
	divmodw
	pop
	pop
	swap
	!
	assert
	<=
	assert
label39:
	txna ApplicationArgs 1
	btoi
	int 1
	==
	bnz label27
	global GroupSize
	int 1
	-
	gtxns TypeEnum
	intc 6 
	==
	assert
	global GroupSize
	int 1
	-
	gtxns XferAsset
	txna ApplicationArgs 1
	btoi
	==
	assert
	global GroupSize
	int 1
	-
	gtxns AssetReceiver
	global CurrentApplicationAddress
	==
	assert
	global GroupSize
	int 1
	-
	gtxns AssetAmount
	int 0
	>
	assert
	global GroupSize
	int 1
	-
	gtxns AssetAmount
	txna ApplicationArgs 2
	btoi
	load 22
	+
	==
	assert
label38:
	txna ApplicationArgs 1
	btoi
	byte base64 YTE=
	app_global_get
	==
	bnz label28
	byte base64 YTI=
	app_global_get
	txna ApplicationArgs 2
	btoi
	callsub label29
label36:
	txna ApplicationArgs 1
	btoi
	byte base64 YTE=
	app_global_get
	==
	bnz label30
	byte base64 YjI=
	byte base64 YjI=
	app_global_get
	load 22
	+
	app_global_put
label34:
	load 22
	bytec 5 
	app_global_get
	mulw
	int 0
	intc 4 
	divmodw
	pop
	pop
	swap
	!
	assert
	store 23
	txna ApplicationArgs 1
	btoi
	byte base64 YTE=
	app_global_get
	==
	bnz label31
	byte base64 YjI=
	byte base64 YjI=
	app_global_get
	load 23
	-
	app_global_put
	bytec 7 
	bytec 7 
	app_global_get
	load 23
	+
	app_global_put
	bytec 14 
	bytec 14 
	app_global_get
	load 22
	load 23
	-
	callsub label32
	app_global_put
label33:
	byte base64 YjE=
	app_global_get
	intc 9 
	>=
	assert
	byte base64 YjI=
	app_global_get
	intc 9 
	>=
	assert
	byte base64 YjE=
	app_global_get
	byte base64 YjI=
	app_global_get
	/
	intc 7 
	<
	assert
	byte base64 YjI=
	app_global_get
	byte base64 YjE=
	app_global_get
	/
	intc 7 
	<
	assert
	int 1
	b label10
label31:
	byte base64 YjE=
	byte base64 YjE=
	app_global_get
	load 23
	-
	app_global_put
	bytec 6 
	bytec 6 
	app_global_get
	load 23
	+
	app_global_put
	bytec 13 
	bytec 13 
	app_global_get
	load 22
	load 23
	-
	callsub label32
	app_global_put
	b label33
label30:
	byte base64 YjE=
	byte base64 YjE=
	app_global_get
	load 22
	+
	app_global_put
	b label34
label28:
	byte base64 YTE=
	app_global_get
	int 1
	==
	bnz label35
	byte base64 YTE=
	app_global_get
	txna ApplicationArgs 2
	btoi
	callsub label29
	b label36
label35:
	txna ApplicationArgs 2
	btoi
	callsub label37
	b label36
label27:
	global GroupSize
	int 1
	-
	gtxns TypeEnum
	int 1
	==
	assert
	global GroupSize
	int 1
	-
	gtxns Receiver
	global CurrentApplicationAddress
	==
	assert
	global GroupSize
	int 1
	-
	gtxns Amount
	int 0
	>
	assert
	global GroupSize
	int 1
	-
	gtxns Amount
	txna ApplicationArgs 2
	btoi
	load 22
	+
	==
	assert
	b label38
label26:
	txna ApplicationArgs 2
	btoi
	byte base64 YjE=
	app_global_get
	bytec 8 
	app_global_get
	mulw
	int 0
	intc 4 
	divmodw
	pop
	pop
	swap
	!
	assert
	<=
	assert
	b label39
label24:
	txn OnCompletion
	int 0
	==
	assert
	txn TypeEnum
	int 6
	==
	assert
	txn GroupIndex
	int 1
	-
	gtxns OnCompletion
	int 0
	==
	assert
	txn GroupIndex
	int 1
	-
	gtxns TypeEnum
	int 6
	==
	assert
	txn GroupIndex
	int 1
	-
	gtxns ApplicationID
	global CurrentApplicationID
	==
	assert
	txn GroupIndex
	int 1
	-
	gtxnsa ApplicationArgs 0
	bytec 18 
	==
	assert
	txn GroupIndex
	int 1
	-
	gloads 14
	int 0
	>
	bnz label40
label42:
	int 1
	b label10
label40:
	txn GroupIndex
	int 1
	-
	gloads 8
	bnz label41
	byte base64 YTI=
	app_global_get
	txn GroupIndex
	int 1
	-
	gloads 14
	callsub label29
	b label42
label41:
	byte base64 YTE=
	app_global_get
	int 1
	==
	bnz label43
	byte base64 YTE=
	app_global_get
	txn GroupIndex
	int 1
	-
	gloads 14
	callsub label29
	b label42
label43:
	txn GroupIndex
	int 1
	-
	gloads 14
	callsub label37
	b label42
label23:
	global LatestTimestamp
	bytec 17 
	app_global_get
	-
	store 24
	bytec 17 
	global LatestTimestamp
	app_global_put
	byte base64 YjI=
	app_global_get
	intc 7 
	mulw
	int 0
	byte base64 YjE=
	app_global_get
	divmodw
	pop
	pop
	swap
	!
	assert
	store 25
	byte base64 YjE=
	app_global_get
	intc 7 
	mulw
	int 0
	byte base64 YjI=
	app_global_get
	divmodw
	pop
	pop
	swap
	!
	assert
	store 26
	intc 8 
	load 25
	/
	load 24
	>
	bnz label44
label68:
	intc 8 
	load 26
	/
	load 24
	>
	bnz label45
label67:
	intc 5 
	bytec 5 
	app_global_get_ex
	store 31
	store 32
	load 31
	assert
	load 32
	intc 4 
	<=
	assert
	bytec 5 
	load 32
	app_global_put
	txn GroupIndex
	int 1
	-
	gtxns TypeEnum
	int 1
	==
	bnz label46
	txn GroupIndex
	int 1
	-
	gtxns XferAsset
	byte base64 YTE=
	app_global_get
	==
	txn GroupIndex
	int 1
	-
	gtxns XferAsset
	byte base64 YTI=
	app_global_get
	==
	||
	assert
	txn GroupIndex
	int 1
	-
	gtxns TypeEnum
	intc 6 
	==
	assert
	txn GroupIndex
	int 1
	-
	gtxns XferAsset
	txn GroupIndex
	int 1
	-
	gtxns XferAsset
	==
	assert
	txn GroupIndex
	int 1
	-
	gtxns AssetReceiver
	global CurrentApplicationAddress
	==
	assert
	txn GroupIndex
	int 1
	-
	gtxns AssetAmount
	int 0
	>
	assert
	txn GroupIndex
	int 1
	-
	gtxns AssetAmount
	store 9
	txn GroupIndex
	int 1
	-
	gtxns XferAsset
	byte base64 YTE=
	app_global_get
	==
	bnz label47
	int 0
label65:
	store 8
label66:
	txn OnCompletion
	int 0
	==
	assert
	txn TypeEnum
	int 6
	==
	assert
	txna ApplicationArgs 0
	bytec 18 
	==
	bnz label48
label64:
	txna ApplicationArgs 0
	bytec 18 
	==
	bnz label49
	load 9
	intc 10 
	mulw
	int 0
	intc 4 
	divmodw
	pop
	pop
	swap
	!
	assert
	int 1
	+
	store 3
	load 9
	load 3
	-
	store 10
	load 10
	int 0
	>
	assert
	load 8
	bnz label50
	byte base64 YjE=
	app_global_get
	load 10
	mulw
	int 0
	byte base64 YjI=
	app_global_get
	load 10
	+
	divmodw
	pop
	pop
	swap
	!
	assert
	store 2
	byte base64 YjE=
	byte base64 YjE=
	app_global_get
	load 2
	-
	app_global_put
	byte base64 YjI=
	byte base64 YjI=
	app_global_get
	load 9
	+
	app_global_put
	byte base64 YTE=
	app_global_get
	int 1
	==
	bnz label51
	byte base64 YTE=
	app_global_get
	load 2
	callsub label29
label55:
	load 2
	load 10
	callsub label52
label56:
	load 2
	int 0
	>
	assert
	load 2
	txna ApplicationArgs 1
	btoi
	>=
	assert
label60:
	load 3
	bytec 5 
	app_global_get
	mulw
	int 0
	intc 4 
	divmodw
	pop
	pop
	swap
	!
	assert
	store 23
	load 8
	bnz label53
	byte base64 YjI=
	byte base64 YjI=
	app_global_get
	load 23
	-
	app_global_put
	bytec 7 
	bytec 7 
	app_global_get
	load 23
	+
	app_global_put
	bytec 14 
	bytec 14 
	app_global_get
	load 3
	load 23
	-
	callsub label32
	app_global_put
label54:
	byte base64 YjE=
	app_global_get
	intc 9 
	>=
	assert
	byte base64 YjI=
	app_global_get
	intc 9 
	>=
	assert
	byte base64 YjE=
	app_global_get
	byte base64 YjI=
	app_global_get
	/
	intc 7 
	<
	assert
	byte base64 YjI=
	app_global_get
	byte base64 YjE=
	app_global_get
	/
	intc 7 
	<
	assert
	int 1
	b label10
label53:
	byte base64 YjE=
	byte base64 YjE=
	app_global_get
	load 23
	-
	app_global_put
	bytec 6 
	bytec 6 
	app_global_get
	load 23
	+
	app_global_put
	bytec 13 
	bytec 13 
	app_global_get
	load 3
	load 23
	-
	callsub label32
	app_global_put
	b label54
label51:
	load 2
	callsub label37
	b label55
label50:
	byte base64 YjI=
	app_global_get
	load 10
	mulw
	int 0
	byte base64 YjE=
	app_global_get
	load 10
	+
	divmodw
	pop
	pop
	swap
	!
	assert
	store 2
	byte base64 YjE=
	byte base64 YjE=
	app_global_get
	load 9
	+
	app_global_put
	byte base64 YjI=
	byte base64 YjI=
	app_global_get
	load 2
	-
	app_global_put
	byte base64 YTI=
	app_global_get
	load 2
	callsub label29
	load 10
	load 2
	callsub label52
	b label56
label49:
	txna ApplicationArgs 1
	btoi
	store 11
	load 11
	int 0
	>
	assert
	load 8
	bnz label57
	byte base64 YjI=
	app_global_get
	load 11
	mulw
	int 0
	byte base64 YjE=
	app_global_get
	load 11
	-
	divmodw
	pop
	pop
	swap
	!
	assert
	int 1
	+
	store 12
label63:
	load 12
	int 0
	>
	assert
	load 12
	intc 4 
	mulw
	int 0
	intc 4 
	intc 10 
	-
	divmodw
	pop
	pop
	swap
	!
	assert
	int 1
	+
	load 12
	-
	store 3
	load 12
	load 3
	+
	store 13
	load 9
	load 13
	>=
	assert
	load 8
	bnz label58
	byte base64 YjE=
	byte base64 YjE=
	app_global_get
	load 11
	-
	app_global_put
	byte base64 YjI=
	byte base64 YjI=
	app_global_get
	load 13
	+
	app_global_put
	byte base64 YTE=
	app_global_get
	int 1
	==
	bnz label59
	byte base64 YTE=
	app_global_get
	load 11
	callsub label29
label61:
	load 11
	load 12
	callsub label52
label62:
	load 9
	load 13
	-
	store 14
	b label60
label59:
	load 11
	callsub label37
	b label61
label58:
	byte base64 YjE=
	byte base64 YjE=
	app_global_get
	load 13
	+
	app_global_put
	byte base64 YjI=
	byte base64 YjI=
	app_global_get
	load 11
	-
	app_global_put
	byte base64 YTI=
	app_global_get
	load 11
	callsub label29
	load 12
	load 11
	callsub label52
	b label62
label57:
	byte base64 YjE=
	app_global_get
	load 11
	mulw
	int 0
	byte base64 YjI=
	app_global_get
	load 11
	-
	divmodw
	pop
	pop
	swap
	!
	assert
	int 1
	+
	store 12
	b label63
label48:
	txn GroupIndex
	int 1
	+
	gtxns OnCompletion
	int 0
	==
	assert
	txn GroupIndex
	int 1
	+
	gtxns TypeEnum
	int 6
	==
	assert
	txn GroupIndex
	int 1
	+
	gtxns ApplicationID
	global CurrentApplicationID
	==
	assert
	txn GroupIndex
	int 1
	+
	gtxnsa ApplicationArgs 0
	bytec 33 
	==
	assert
	b label64
label47:
	int 1
	b label65
label46:
	byte base64 YTE=
	app_global_get
	int 1
	==
	assert
	txn GroupIndex
	int 1
	-
	gtxns TypeEnum
	int 1
	==
	assert
	txn GroupIndex
	int 1
	-
	gtxns Receiver
	global CurrentApplicationAddress
	==
	assert
	txn GroupIndex
	int 1
	-
	gtxns Amount
	int 0
	>
	assert
	txn GroupIndex
	int 1
	-
	gtxns Amount
	store 9
	int 1
	store 8
	b label66
label45:
	bytec 20 
	bytec 20 
	app_global_get
	load 26
	load 24
	*
	callsub label32
	app_global_put
	b label67
label44:
	bytec 19 
	bytec 19 
	app_global_get
	load 25
	load 24
	*
	callsub label32
	app_global_put
	b label68
label22:
	txn GroupIndex
	int 2
	-
	gtxns TypeEnum
	intc 6 
	==
	assert
	txn GroupIndex
	int 2
	-
	gtxns XferAsset
	bytec 16 
	app_global_get
	==
	assert
	txn GroupIndex
	int 2
	-
	gtxns AssetReceiver
	global CurrentApplicationAddress
	==
	assert
	txn GroupIndex
	int 2
	-
	gtxns AssetAmount
	int 0
	>
	assert
	txn GroupIndex
	int 1
	-
	gtxns OnCompletion
	int 0
	==
	assert
	txn GroupIndex
	int 1
	-
	gtxns TypeEnum
	int 6
	==
	assert
	txn GroupIndex
	int 1
	-
	gtxns ApplicationID
	global CurrentApplicationID
	==
	assert
	txn GroupIndex
	int 1
	-
	gtxnsa ApplicationArgs 0
	bytec 31 
	==
	assert
	txn OnCompletion
	int 0
	==
	assert
	txn TypeEnum
	int 6
	==
	assert
	txn GroupIndex
	int 2
	-
	gtxns AssetAmount
	bytec 4 
	app_global_get
	==
	bnz label69
	txn GroupIndex
	int 2
	-
	gtxns AssetAmount
	byte base64 YjI=
	app_global_get
	mulw
	int 0
	bytec 4 
	app_global_get
	divmodw
	pop
	pop
	swap
	!
	assert
	store 7
label70:
	load 7
	int 0
	>
	assert
	load 7
	byte base64 YjI=
	app_global_get
	<=
	assert
	byte base64 YjI=
	byte base64 YjI=
	app_global_get
	load 7
	-
	app_global_put
	bytec 4 
	bytec 4 
	app_global_get
	txn GroupIndex
	int 2
	-
	gtxns AssetAmount
	-
	app_global_put
	byte base64 YTI=
	app_global_get
	load 7
	callsub label29
	int 1
	b label10
label69:
	byte base64 YjI=
	app_global_get
	store 7
	b label70
label21:
	txn GroupIndex
	int 1
	-
	gtxns TypeEnum
	intc 6 
	==
	assert
	txn GroupIndex
	int 1
	-
	gtxns XferAsset
	bytec 16 
	app_global_get
	==
	assert
	txn GroupIndex
	int 1
	-
	gtxns AssetReceiver
	global CurrentApplicationAddress
	==
	assert
	txn GroupIndex
	int 1
	-
	gtxns AssetAmount
	int 0
	>
	assert
	txn OnCompletion
	int 0
	==
	assert
	txn TypeEnum
	int 6
	==
	assert
	txn GroupIndex
	int 1
	+
	gtxns OnCompletion
	int 0
	==
	assert
	txn GroupIndex
	int 1
	+
	gtxns TypeEnum
	int 6
	==
	assert
	txn GroupIndex
	int 1
	+
	gtxns ApplicationID
	global CurrentApplicationID
	==
	assert
	txn GroupIndex
	int 1
	+
	gtxnsa ApplicationArgs 0
	bytec 32 
	==
	assert
	txn GroupIndex
	int 1
	-
	gtxns AssetAmount
	bytec 4 
	app_global_get
	==
	bnz label71
	txn GroupIndex
	int 1
	-
	gtxns AssetAmount
	byte base64 YjE=
	app_global_get
	mulw
	int 0
	bytec 4 
	app_global_get
	divmodw
	pop
	pop
	swap
	!
	assert
	store 6
label74:
	load 6
	int 0
	>
	assert
	load 6
	byte base64 YjE=
	app_global_get
	<=
	assert
	byte base64 YjE=
	byte base64 YjE=
	app_global_get
	load 6
	-
	app_global_put
	byte base64 YTE=
	app_global_get
	int 1
	==
	bnz label72
	byte base64 YTE=
	app_global_get
	load 6
	callsub label29
label73:
	int 1
	b label10
label72:
	load 6
	callsub label37
	b label73
label71:
	byte base64 YjE=
	app_global_get
	store 6
	b label74
label20:
	txn OnCompletion
	int 0
	==
	assert
	txn TypeEnum
	int 6
	==
	assert
	txn GroupIndex
	int 2
	-
	gtxns OnCompletion
	int 0
	==
	assert
	txn GroupIndex
	int 2
	-
	gtxns TypeEnum
	int 6
	==
	assert
	txn GroupIndex
	int 2
	-
	gtxns ApplicationID
	global CurrentApplicationID
	==
	assert
	txn GroupIndex
	int 2
	-
	gtxnsa ApplicationArgs 0
	bytec 25 
	==
	assert
	txn GroupIndex
	int 2
	-
	gloads 20
	int 0
	>
	bnz label75
label77:
	int 1
	b label10
label75:
	int 0
	bnz label76
	byte base64 YTI=
	app_global_get
	txn GroupIndex
	int 2
	-
	gloads 20
	callsub label29
	b label77
label76:
	byte base64 YTE=
	app_global_get
	int 1
	==
	bnz label78
	byte base64 YTE=
	app_global_get
	txn GroupIndex
	int 2
	-
	gloads 20
	callsub label29
	b label77
label78:
	txn GroupIndex
	int 2
	-
	gloads 20
	callsub label37
	b label77
label19:
	txn OnCompletion
	int 0
	==
	assert
	txn TypeEnum
	int 6
	==
	assert
	txn GroupIndex
	int 1
	-
	gtxns OnCompletion
	int 0
	==
	assert
	txn GroupIndex
	int 1
	-
	gtxns TypeEnum
	int 6
	==
	assert
	txn GroupIndex
	int 1
	-
	gtxns ApplicationID
	global CurrentApplicationID
	==
	assert
	txn GroupIndex
	int 1
	-
	gtxnsa ApplicationArgs 0
	bytec 25 
	==
	assert
	txn GroupIndex
	int 1
	-
	gloads 19
	int 0
	>
	bnz label79
label81:
	int 1
	b label10
label79:
	int 1
	bnz label80
	byte base64 YTI=
	app_global_get
	txn GroupIndex
	int 1
	-
	gloads 19
	callsub label29
	b label81
label80:
	byte base64 YTE=
	app_global_get
	int 1
	==
	bnz label82
	byte base64 YTE=
	app_global_get
	txn GroupIndex
	int 1
	-
	gloads 19
	callsub label29
	b label81
label82:
	txn GroupIndex
	int 1
	-
	gloads 19
	callsub label37
	b label81
label18:
	byte base64 YTE=
	app_global_get
	int 1
	==
	bnz label83
	txn GroupIndex
	int 2
	-
	gtxns TypeEnum
	intc 6 
	==
	assert
	txn GroupIndex
	int 2
	-
	gtxns XferAsset
	byte base64 YTE=
	app_global_get
	==
	assert
	txn GroupIndex
	int 2
	-
	gtxns AssetReceiver
	global CurrentApplicationAddress
	==
	assert
	txn GroupIndex
	int 2
	-
	gtxns AssetAmount
	int 0
	>
	assert
	txn GroupIndex
	int 2
	-
	gtxns AssetAmount
	store 4
label95:
	txn GroupIndex
	int 1
	-
	gtxns TypeEnum
	intc 6 
	==
	assert
	txn GroupIndex
	int 1
	-
	gtxns XferAsset
	byte base64 YTI=
	app_global_get
	==
	assert
	txn GroupIndex
	int 1
	-
	gtxns AssetReceiver
	global CurrentApplicationAddress
	==
	assert
	txn GroupIndex
	int 1
	-
	gtxns AssetAmount
	int 0
	>
	assert
	txn GroupIndex
	int 1
	-
	gtxns AssetAmount
	store 5
	txn OnCompletion
	int 0
	==
	assert
	txn TypeEnum
	int 6
	==
	assert
	txn GroupIndex
	int 1
	+
	gtxns OnCompletion
	int 0
	==
	assert
	txn GroupIndex
	int 1
	+
	gtxns TypeEnum
	int 6
	==
	assert
	txn GroupIndex
	int 1
	+
	gtxns ApplicationID
	global CurrentApplicationID
	==
	assert
	txn GroupIndex
	int 1
	+
	gtxnsa ApplicationArgs 0
	bytec 29 
	==
	assert
	txn GroupIndex
	int 2
	+
	gtxns OnCompletion
	int 0
	==
	assert
	txn GroupIndex
	int 2
	+
	gtxns TypeEnum
	int 6
	==
	assert
	txn GroupIndex
	int 2
	+
	gtxns ApplicationID
	global CurrentApplicationID
	==
	assert
	txn GroupIndex
	int 2
	+
	gtxnsa ApplicationArgs 0
	bytec 30 
	==
	assert
	byte base64 YjE=
	app_global_get
	byte base64 YjI=
	app_global_get
	+
	int 0
	==
	bnz label84
	byte base64 YjE=
	app_global_get
	intc 7 
	mulw
	int 0
	byte base64 YjI=
	app_global_get
	divmodw
	pop
	pop
	swap
	!
	assert
	store 15
	load 4
	intc 7 
	mulw
	int 0
	load 5
	divmodw
	pop
	pop
	swap
	!
	assert
	store 16
	load 15
	intc 4 
	mulw
	int 0
	load 16
	divmodw
	pop
	pop
	swap
	!
	assert
	store 21
	load 21
	intc 4 
	txna ApplicationArgs 1
	btoi
	-
	>
	load 21
	intc 4 
	txna ApplicationArgs 1
	btoi
	+
	<
	&&
	assert
	load 16
	load 15
	>
	bnz label85
	load 16
	load 15
	<
	bnz label86
	load 16
	load 15
	==
	bnz label87
	err
label87:
	load 4
	store 17
	load 5
	store 18
	int 0
	store 19
	int 0
	store 20
label94:
	byte base64 YjE=
	app_global_get
	byte base64 YjI=
	app_global_get
	+
	int 0
	==
	bnz label88
	load 17
	bytec 4 
	app_global_get
	mulw
	int 0
	byte base64 YjE=
	app_global_get
	divmodw
	pop
	pop
	swap
	!
	assert
	store 28
	load 18
	bytec 4 
	app_global_get
	mulw
	int 0
	byte base64 YjI=
	app_global_get
	divmodw
	pop
	pop
	swap
	!
	assert
	store 29
	load 28
	load 29
	>
	bnz label89
	load 28
	store 0
label92:
	load 0
	int 0
	>
	assert
	byte base64 YjE=
	app_global_get
	byte base64 YjI=
	app_global_get
	+
	int 0
	==
	bnz label90
label91:
	byte base64 YjE=
	byte base64 YjE=
	app_global_get
	load 17
	+
	app_global_put
	byte base64 YjI=
	byte base64 YjI=
	app_global_get
	load 18
	+
	app_global_put
	bytec 4 
	bytec 4 
	app_global_get
	load 0
	+
	app_global_put
	bytec 16 
	app_global_get
	load 0
	callsub label29
	byte base64 YjE=
	app_global_get
	intc 9 
	>=
	assert
	byte base64 YjI=
	app_global_get
	intc 9 
	>=
	assert
	byte base64 YjE=
	app_global_get
	byte base64 YjI=
	app_global_get
	/
	intc 7 
	<
	assert
	byte base64 YjI=
	app_global_get
	byte base64 YjE=
	app_global_get
	/
	intc 7 
	<
	assert
	int 1
	b label10
label90:
	bytec 17 
	global LatestTimestamp
	app_global_put
	b label91
label89:
	load 29
	store 0
	b label92
label88:
	intc 8 
	load 17
	/
	load 18
	>
	bnz label93
	load 17
	sqrt
	load 18
	sqrt
	*
	store 0
	b label92
label93:
	load 17
	load 18
	*
	sqrt
	store 0
	b label92
label86:
	load 4
	store 17
	load 4
	byte base64 YjI=
	app_global_get
	mulw
	int 0
	byte base64 YjE=
	app_global_get
	divmodw
	pop
	pop
	swap
	!
	assert
	int 1
	+
	store 18
	int 0
	store 19
	load 5
	load 18
	-
	store 20
	b label94
label85:
	load 5
	byte base64 YjE=
	app_global_get
	mulw
	int 0
	byte base64 YjI=
	app_global_get
	divmodw
	pop
	pop
	swap
	!
	assert
	int 1
	+
	store 17
	load 5
	store 18
	load 4
	load 17
	-
	store 19
	int 0
	store 20
	b label94
label84:
	load 4
	store 17
	load 5
	store 18
	b label94
label83:
	txn GroupIndex
	int 2
	-
	gtxns TypeEnum
	int 1
	==
	assert
	txn GroupIndex
	int 2
	-
	gtxns Receiver
	global CurrentApplicationAddress
	==
	assert
	txn GroupIndex
	int 2
	-
	gtxns Amount
	int 0
	>
	assert
	txn GroupIndex
	int 2
	-
	gtxns Amount
	store 4
	b label95
label17:
	int 1
	b label10
label15:
	txn OnCompletion
	intc 6 
	==
	bnz label96
	txna ApplicationArgs 0
	pushbytes 0x736375 
	==
	bnz label97
	txna ApplicationArgs 0
	pushbytes 0x7272 
	==
	bnz label98
	err
label98:
	txn Sender
	bytec 9 
	app_global_get
	==
	assert
	byte base64 YTE=
	app_global_get
	int 1
	==
	bnz label99
	byte base64 YTE=
	app_global_get
	bytec 6 
	app_global_get
	callsub label29
label100:
	byte base64 YTI=
	app_global_get
	bytec 7 
	app_global_get
	callsub label29
	bytec 6 
	int 0
	app_global_put
	bytec 7 
	int 0
	app_global_put
	int 1
	b label10
label99:
	bytec 6 
	app_global_get
	callsub label37
	b label100
label97:
	txn Sender
	bytec 9 
	app_global_get
	==
	assert
	intc 5 
	bytec 12 
	app_global_get_ex
	store 33
	store 34
	load 33
	bnz label101
label102:
	txna ApplicationArgs 1
	btoi
	global LatestTimestamp
	bytec 12 
	app_global_get
	+
	>=
	assert
	bytec 11 
	txna ApplicationArgs 1
	btoi
	app_global_put
	int 1
	b label10
label101:
	bytec 12 
	load 34
	app_global_put
	b label102
label96:
	txn Sender
	bytec 9 
	app_global_get
	==
	assert
	bytec 11 
	app_global_get
	int 0
	!=
	assert
	bytec 11 
	app_global_get
	global LatestTimestamp
	<=
	assert
	bytec 11 
	int 0
	app_global_put
	int 1
	b label10
label4:
	int 0
	b label10
label3:
	int 0
	b label10
label2:
	int 0
	b label10
label1:
	txn GlobalNumByteSlice
	intc 6 
	>=
	assert
	txn GlobalNumUint
	pushint 32
	>=
	assert
	intc 5 
	bytec 9 
	app_global_get_ex
	store 1
	store 30
	load 1
	assert
	bytec 9 
	load 30
	app_global_put
	txna ApplicationArgs 0
	btoi
	int 0
	!=
	txna ApplicationArgs 1
	btoi
	int 0
	!=
	&&
	assert
	txna ApplicationArgs 0
	btoi
	txna ApplicationArgs 1
	btoi
	<
	assert
	byte base64 YTE=
	txna ApplicationArgs 0
	btoi
	app_global_put
	byte base64 YTI=
	txna ApplicationArgs 1
	btoi
	app_global_put
	pushbytes 0x7669 
	txna ApplicationArgs 2
	btoi
	app_global_put
	bytec 15 
	int 0
	app_global_put
	int 1
label10:
	return
label32:
	store 44
	store 43
	load 44
	intc 8 
	load 43
	-
	>
	bnz label103
	load 43
	load 44
	+
	retsub
label103:
	load 44
	intc 8 
	load 43
	-
	-
	int 1
	-
	retsub
label12:
	store 45
	itxn_begin
	intc 6 
	itxn_field TypeEnum
	load 45
	itxn_field XferAsset
	int 0
	itxn_field AssetAmount
	global CurrentApplicationAddress
	itxn_field AssetReceiver
	int 0
	itxn_field Fee
	itxn_submit
	retsub
label29:
	store 47
	store 46
	itxn_begin
	intc 6 
	itxn_field TypeEnum
	load 46
	itxn_field XferAsset
	load 47
	itxn_field AssetAmount
	txn Sender
	itxn_field AssetReceiver
	int 0
	itxn_field Fee
	itxn_submit
	retsub
label37:
	store 48
	global CurrentApplicationAddress
	balance
	load 48
	global MinBalance
	+
	>=
	assert
	itxn_begin
	int 1
	itxn_field TypeEnum
	load 48
	itxn_field Amount
	txn Sender
	itxn_field Receiver
	int 0
	itxn_field Fee
	itxn_submit
	retsub
label52:
	store 50
	store 49
	bytec 21 
	bytec 21 
	app_global_get
	load 49
	callsub label32
	app_global_put
	bytec 22 
	bytec 22 
	app_global_get
	load 50
	callsub label32
	app_global_put
	intc 8 
	load 50
	/
	load 25
	>
	bnz label104
label106:
	intc 8 
	load 49
	/
	load 26
	>
	bz label105
	bytec 24 
	bytec 24 
	app_global_get
	load 49
	load 26
	*
	callsub label32
	app_global_put
	b label105
label104:
	bytec 23 
	bytec 23 
	app_global_get
	load 50
	load 25
	*
	callsub label32
	app_global_put
	b label106
label105:
	retsub
ASSEMBLY
   ))

(pretty-print postmodern-assembly)
(pretty-print board-assembly)
