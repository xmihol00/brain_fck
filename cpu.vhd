-- cpu.vhd: Simple 8-bit CPU (BrainF*ck interpreter)
-- Copyright (C) 2020 Brno University of Technology,
--                    Faculty of Information Technology
-- Author(s): DAVID MIHOLA, xmihol00@stud.fit.vutbr.cz
--

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

-- ----------------------------------------------------------------------------
--                        Entity declaration
-- ----------------------------------------------------------------------------
entity cpu is
 port (
   CLK   : in std_logic;  -- hodinovy signal
   RESET : in std_logic;  -- asynchronni reset procesoru
   EN    : in std_logic;  -- povoleni cinnosti procesoru
 
   -- synchronni pamet ROM
   CODE_ADDR : out std_logic_vector(11 downto 0); -- adresa do pameti
   CODE_DATA : in std_logic_vector(7 downto 0);   -- CODE_DATA <- rom[CODE_ADDR] pokud CODE_EN='1'
   CODE_EN   : out std_logic;                     -- povoleni cinnosti
   
   -- synchronni pamet RAM
   DATA_ADDR  : out std_logic_vector(9 downto 0); -- adresa do pameti
   DATA_WDATA : out std_logic_vector(7 downto 0); -- ram[DATA_ADDR] <- DATA_WDATA pokud DATA_EN='1'
   DATA_RDATA : in std_logic_vector(7 downto 0);  -- DATA_RDATA <- ram[DATA_ADDR] pokud DATA_EN='1'
   DATA_WE    : out std_logic;                    -- cteni (0) / zapis (1)
   DATA_EN    : out std_logic;                    -- povoleni cinnosti 
   
   -- vstupni port
   IN_DATA   : in std_logic_vector(7 downto 0);   -- IN_DATA <- stav klavesnice pokud IN_VLD='1' a IN_REQ='1'
   IN_VLD    : in std_logic;                      -- data platna
   IN_REQ    : out std_logic;                     -- pozadavek na vstup data
   
   -- vystupni port
   OUT_DATA : out  std_logic_vector(7 downto 0);  -- zapisovana data
   OUT_BUSY : in std_logic;                       -- LCD je zaneprazdnen (1), nelze zapisovat
   OUT_WE   : out std_logic                       -- LCD <- OUT_DATA pokud OUT_WE='1' a OUT_BUSY='0'
 );
end cpu;


-- ----------------------------------------------------------------------------
--                      Architecture declaration
-- ----------------------------------------------------------------------------
architecture behavioral of cpu is

	-- Program Counter (PC)
	signal PCr : std_logic_vector(11 downto 0); -- pointer to the source code memory (ROM)
	signal PCi : std_logic; -- incrementing the PC
	signal PCd : std_logic; -- decrementing the PC
	signal PCl : std_logic; -- load from memory
	-- PC end

	-- Register Address Stack (RAS)
	type STACK is array (15 downto 0) of std_logic_vector(11 downto 0); -- typedef of an array with addresses of loop headers
	signal RAStop : std_logic_vector(11 downto 0) := "000000000000"; -- top of the adress stack
	signal RASph : std_logic; -- push address on the stack
	signal RASpp : std_logic; -- pop adress from the stack
	signal RASst : STACK;
	signal RASptr : integer range 0 to 15;
	-- RAS end

	-- Counter (CNT)
	signal CNTr : std_logic_vector(3 downto 0); -- level of loop immersion
	signal CNTi : std_logic; -- increment the CNT
	signal CNTd : std_logic; -- decrement the CNT
	-- CNT end

	-- RAM pointer (PTR)
	signal PTRr : std_logic_vector(9 downto 0); -- pointer to the programm memory (RAM)
	signal PTRi : std_logic; -- increment the PTR
	signal PTRd : std_logic; -- decrement the PTR
	-- PTR end

	-- Multiplexor (MUX)
	signal MUXsel : std_logic_vector(1 downto 0); -- multiplexor sellector
	-- MUX end

	-- FSM states
	type states_FSM is (
			start,
			fetch,
			decode,
			data_inc_exe,
			data_dec_exe,
			loop_inc_exe,
			loop_skip,
			loop_dec_exe,
			write_exe,
			load_exe,
			prog_end
		);
	-- FSM states end

	signal p_state : states_FSM := start; -- FSM present state
	signal n_state : states_FSM := start; -- FSM next state

begin
	-- PC
	program_counter : process (PCi, PCd, PCl, CLK, RESET)
	begin
		if RESET = '1' then
			PCr <= "000000000000"; -- reseting PC register on reset
		elsif rising_edge(CLK) then
			if PCi = '1' then
				PCr <= PCr + 1; -- PC increment
			elsif PCd = '1' then
				PCr <= PCr - 1; -- PC decrement
			elsif PCl = '1' then
				PCr <= RAStop;
			end if;
		end if;	

	end process;
	CODE_ADDR <= PCr;
	-- PC end

	-- RAS
	register_adress_stack : process (RASph, RASpp, CLK, RESET)
		variable ras_ptr : integer range 0 to 15;
	begin
		if RESET = '1' then
			RASptr <= 15;
			RASst <= (others => "000000000000");
		elsif rising_edge(CLK) then
			if RASph = '1' then
				if RASptr = 15 then -- rotate RASptr
					ras_ptr := 0;
				else
					ras_ptr := RASptr + 1;
				end if;
				RASst(ras_ptr) <= PCr;
				RAStop <= PCr;
				RASptr <= ras_ptr;
			elsif RASpp = '1' then
				if RASptr = 0 then -- rotate RASptr
					ras_ptr := 15;
				else
					ras_ptr := RASptr - 1;
				end if;
				RAStop <= RASst(ras_ptr);
				RASptr <= ras_ptr;
			end if;
		end if;
	end process;
	-- RAS end

	-- CNT
	counter : process (CNTi, CNTd, CLK, RESET)
	begin
		-- counter logic
		if RESET = '1' then
			CNTr <= "0000";
		elsif rising_edge(CLK) then
			if CNTi = '1' then
				CNTr <= CNTr + 1;
			elsif CNTd = '1' then
				CNTr <= CNTr - 1;
			end if;
		end if;
	end process;
	-- CNT end

	-- PTR
	RAM_pointer : process (PTRi, PTRd, CLK, RESET)
	begin
		-- pointer logic
		if RESET = '1' then
			PTRr <= "0000000000";
		elsif rising_edge(CLK) then
			if PTRi = '1' then
				PTRr <= PTRr + 1;
			elsif PTRd = '1' then
				PTRr <= PTRr - 1;
			end if;
		end if;
	end process;
	DATA_ADDR <= PTRr;
	-- PTR end

	-- MUX
        DATA_WDATA <= IN_DATA        when MUXsel = "11" else
	     	      DATA_RDATA + 1 when MUXsel = "10" else
      	     	      DATA_RDATA - 1 when MUXsel = "01" else
	    	      "00000000";
	-- MUX end

	OUT_DATA <= DATA_RDATA;
	
	-- Mealy present state logic
	FSM_p_state_logic: process(CLK, RESET, EN) is
	begin
		if RESET = '1' then
			p_state <= start;
		elsif rising_edge(CLK) then
			if EN = '1' then
				p_state <= n_state;
			end if;
		end if;
	end process;
	-- Mealy present state logic end

	-- Mealy next state logic
	FSM_n_state_logic: process(p_state, CNTr, DATA_RDATA, CODE_DATA, IN_VLD, OUT_BUSY) is
	begin
		-- inner signals default values
		PCi <= '0';
		PCd <= '0';
		PCl <= '0';

		RASph <= '0';
		RASpp <= '0';

		CNTi <= '0';
		CNTd <= '0';

		PTRi <= '0';
		PTRd <= '0';

		MUXsel <= "00";
		
		-- out signals default values
		IN_REQ <= '0';
		OUT_WE <= '0';
		CODE_EN <= '0';

		DATA_WE <= '0';
		DATA_EN <= '0';

		-- FSM body
		case p_state is
			when start => -- starting state
				n_state <= fetch;

			when fetch => -- loads instruction from ROM pointed by PCr to CODE_DATA
				n_state <= decode;
				CODE_EN <= '1';

			when decode =>
				case CODE_DATA is -- decodes the current instruction of the program
					when "00111110" => --0x3E >
						n_state <= fetch;
						PCi <= '1';
						PTRi <= '1';

					when "00111100" => --0x3C <
						n_state <= fetch;
						PCi <= '1';
						PTRd <= '1';

					when "00101011" => --0x2B +
						n_state <= data_inc_exe;
						PCi <= '1';
						DATA_EN <= '1';
						DATA_WE <= '0';

					when "00101101" => --0x2D -
						n_state <= data_dec_exe;
						PCi <= '1';
						DATA_EN <= '1';
						DATA_WE <= '0';

					when "01011011" => --0x5B [
						n_state <= loop_inc_exe;
						PCi <= '1';
						DATA_EN <= '1';
						DATA_WE <= '0';

					when "01011101" => --0x5D ]
						n_state <= loop_dec_exe;
						DATA_EN <= '1';
						DATA_WE <= '0';

					when "00101110" => --0x2E .
						n_state <= write_exe;
						PCi <= '1';
						DATA_EN <= '1';
						DATA_WE <= '0';

					when "00101100" => --0x2C ,
						n_state <= load_exe;
						PCi <= '1';
						IN_REQ <= '1';

					when "00000000" => --0x00 
						n_state <= prog_end;

					when others =>     --unrecognized character (comment)
						n_state <= fetch;
						PCi <= '1';
				end case;
			
			when data_inc_exe => -- increases the value at position pointed by PTRr in RAM and fetches the next instruction
				n_state <= decode;
				CODE_EN <= '1';
				DATA_EN <= '1';
				DATA_WE <= '1';
				MUXsel <= "10";				

			when data_dec_exe => -- decreases the value at position pointed by PTRr in RAM and fetches the next instruction
				n_state <= decode;
				CODE_EN <= '1';
				DATA_EN <= '1';
				DATA_WE <= '1';
				MUXsel <= "01";

			when loop_inc_exe => -- executes the start of a loop statement
				if DATA_RDATA = "00000000" then -- false loop, that has to be skipped
					n_state <= loop_skip;
					CNTi <= '1';
					PCi <= '1';
				else				-- true loop, that has to be save on RAS
					n_state <= decode; 
					RASph <= '1';
				end if;
				CODE_EN <= '1'; -- fetches the next instruction
			
			when loop_skip => -- skipes false loop statement and all immersed loops inside
				n_state <= loop_skip;
				PCi <= '1';
				CODE_EN <= '1';
				if CODE_DATA = "01011101" then
					CNTd <= '1';
					if CNTr = "0001" then
						n_state <= fetch;
						PCi <= '0';
					end if;
				elsif CODE_DATA = "01011011" then
					CNTi <= '1';
				end if;

			when loop_dec_exe => -- end of a loop statement
				n_state <= fetch;
				if DATA_RDATA = "00000000" then -- ends the loop statement on false value
					PCi <= '1';
					RASpp <= '1';
				else
					PCl <= '1'; -- loads the start of the loop statement on a true value
				end if;

			when write_exe => -- executes a write operation
				if OUT_BUSY = '1' then -- waits until write is possible
					n_state <= write_exe;
				--	DATA_WE <= '0';
				else		       -- prints the value and fetches the next instruction
					n_state <= decode;
					CODE_EN <= '1';
					OUT_WE <= '1';
				end if;
				-- DATA_EN <= '1';

			when load_exe => -- executes load operation
				if IN_VLD = '0' then -- waits until all characters were entered
					n_state <= load_exe;
					IN_REQ <= '1';
				else  -- saves the read value and fetches next instruction
					n_state <= decode;
					MUXsel <= "11";
					CODE_EN <= '1';
					DATA_EN <= '1';
					DATA_WE <= '1';
				end if;

			when prog_end =>
				n_state <= prog_end;

		end case;
	end process;
	-- Mealy next state logic end

end behavioral;
 
